#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxslt/xslt.h>
#include <libxslt/transform.h>
#include <libxslt/security.h>
#include <libxslt/xsltutils.h>
#include <libexslt/exslt.h>
#include <libexslt/exslt.h>
#include <libxslt/extensions.h>
#include <ei.h>
#include <libxml/xpathInternals.h>


#define CMD_SET_XSLT 1
#define CMD_SET_XML 2
#define CMD_PROCESS 3
#define CMD_REGISTER 4
#define CMD_SET_PARAMS 5
#define RPL_RESULT 0
#define RPL_ERROR 1
#define RPL_CALLBACK 2


void send_length(uint32_t len)
{
  len = htonl(len);
  write(STDOUT_FILENO, &len, 4);
}

void send_byte(int b)
{
  char sbuf[1] = {b};
  write(STDOUT_FILENO, sbuf, 1);
}

void send_buffer(const char *s, uint32_t len)
{
  send_length(len);
  write(STDOUT_FILENO, s, strlen(s));
}

void send_string(const char *s)
{
  send_buffer(s, strlen(s));
}

char **parse_params(char *buf_, int buflen)
{
  static char **params = NULL;
  static char *buf = NULL;
  int i, n = 0;

  if (params)
    free(params);

  if (buf)
    free(buf);
  buf = malloc(buflen);
  memcpy(buf, buf_, buflen);

  /* Count \0 */
  for(i = 0; i < buflen; ++i)
    if (buf[i] == 0)
      n++;

  params = malloc((n + 1) * sizeof(char *));
  params[0] = buf;
  n = 1;
  for(i = 0; i < buflen; ++i)
    if (buf[i] == 0)
    {
      params[n] = buf + i + 1;
      printf("params[%i] = %s\n", n, buf + i + 1);
      n++;
    }
  params[n - 1] = NULL;

  return params;
}

#define ERRBUFSIZE 4096
static char errbuf[ERRBUFSIZE];

void error_func(void *ctx, const char *fmt, ...)
{
  int errbuflen = strlen(errbuf);
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(errbuf + errbuflen, ERRBUFSIZE - errbuflen - 1, fmt, ap);
  va_end(ap);
}

void xpath2ei(ei_x_buff *x, xmlXPathObjectPtr obj, xmlDocPtr doc)
{
  switch(obj->type)
  {
  case XPATH_BOOLEAN:
    ei_x_encode_atom(x, obj->boolval ? "true" : "false");
    break;
  case XPATH_NUMBER:
    if (obj->floatval - (int)obj->floatval != 0)
      ei_x_encode_double(x, obj->floatval);
    else
      ei_x_encode_longlong(x, obj->floatval);
    break;
  case XPATH_STRING:
    ei_x_encode_string(x, (char *)obj->stringval);
    break;
  default:
    ei_x_encode_atom(x, "unknown");
  }
}

void xmlXPathFuncCallback(xmlXPathParserContextPtr ctxt, int nargs)
{
  const xmlChar *name, *xmlns;
  int i;
  ei_x_buff arguments;
  //fprintf(stderr, "callback %i\n",nargs);

  uint32_t rlen;
  int rindex = 0, version;
  char *rbuf = NULL, *s;
  ei_term term;

  xmlXPathObjectPtr ret = NULL;

  if (ctxt == NULL || ctxt->context == NULL)
    return;

  name = ctxt->context->function;
  xmlns = ctxt->context->functionURI;

  ei_x_new_with_version(&arguments);
  ei_x_encode_list_header(&arguments, nargs + 2);
  ei_x_encode_string(&arguments, (char *)xmlns);
  ei_x_encode_string(&arguments, (char *)name);
  for(i = nargs; i > 0; i--)
  {
    xmlXPathObjectPtr obj = valuePop(ctxt);
    xpath2ei(&arguments, obj, ctxt->context->doc);
    xmlXPathFreeObject(obj);
  }
  ei_x_encode_empty_list(&arguments);

  send_length(arguments.index + 1);
  send_byte(RPL_CALLBACK);
  write(STDOUT_FILENO, arguments.buff, arguments.index);
  ei_x_free(&arguments);

  if (read(STDIN_FILENO, &rlen, 4) != 4)
    exit(-1);
  rlen = ntohl(rlen);
  rbuf = malloc(rlen);
  if (read(STDIN_FILENO, rbuf, rlen) != rlen)
    exit(-1);

  ei_decode_version(rbuf, &rindex, &version);
  ei_decode_ei_term(rbuf, &rindex, &term);
  switch(term.ei_type)
  {
  case ERL_ATOM_EXT:
    if (!strcmp(term.value.atom_name, "true"))
      ret = xmlXPathNewBoolean(1);
    else if (!strcmp(term.value.atom_name, "false"))
      ret = xmlXPathNewBoolean(0);
    else
      ret = xmlXPathWrapString(xmlStrdup((xmlChar *)term.value.atom_name));
    break;
  case ERL_SMALL_INTEGER_EXT:
  case ERL_INTEGER_EXT:
    ret = xmlXPathNewFloat(term.value.i_val);
    break;
    //case ERL_LIST_EXT:
  case ERL_STRING_EXT:
    s = xmlMemMalloc(term.size + 1);
    ei_decode_string(rbuf, &rindex, s);
    ret = xmlXPathWrapString(xmlStrdup((xmlChar *)s));
    xmlMemFree(s);
    break;
  case ERL_SMALL_TUPLE_EXT:
    if (term.arity == 2)
    {
      ei_decode_ei_term(rbuf, &rindex, &term);
      if (term.ei_type == ERL_ATOM_EXT && !strcmp(term.value.atom_name, "tree"))
      {
        ei_decode_ei_term(rbuf, &rindex, &term);
        if (term.ei_type == ERL_STRING_EXT)
        {
          s = xmlMemMalloc(term.size + 1);
          ei_decode_string(rbuf, &rindex, s);
          xmlDocPtr doc = xmlParseDoc((xmlChar *)s);
          ret = xmlXPathNewNodeSet((xmlNode *)doc->children);
        }
        else
          fprintf(stderr, "not really tree\n");
      }
      else
        fprintf(stderr, "not tree\n");
    }
    else
      fprintf(stderr, "not 2-tuple\n");
    break;
  default:
    fprintf(stderr, "unknown type: %i\n", term.ei_type);
  }
  valuePush(ctxt, ret);

  free(rbuf);
}

extern int xmlLoadExtDtdDefaultValue;

int main()
{
  uint32_t rlen, running = 1, rbuflen = 0;
  char *rbuf = NULL;
  xsltStylesheetPtr xslt = NULL;
  xmlDocPtr xml = NULL, xsltDoc, res;
  xmlChar *xmlbuf = NULL;
  xsltSecurityPrefsPtr sec;
  int xmlbufsize;
  const char *default_params[] = {NULL};
  char **params = (char **)default_params;
  char *baseuri;
  int baseurilen;
  char *name, *xmlns;

  xmlSetGenericErrorFunc(NULL, error_func);
  xsltSetGenericErrorFunc(NULL, error_func);

  xmlSubstituteEntitiesDefault(0);
  xmlLoadExtDtdDefaultValue = 0;
  sec = xsltNewSecurityPrefs();
  xsltSetSecurityPrefs(sec, XSLT_SECPREF_READ_FILE, xsltSecurityForbid);
  xsltSetSecurityPrefs(sec, XSLT_SECPREF_WRITE_FILE, xsltSecurityForbid);
  xsltSetSecurityPrefs(sec, XSLT_SECPREF_CREATE_DIRECTORY, xsltSecurityForbid);
  xsltSetSecurityPrefs(sec, XSLT_SECPREF_READ_NETWORK, xsltSecurityForbid);
  xsltSetSecurityPrefs(sec, XSLT_SECPREF_WRITE_NETWORK, xsltSecurityForbid);
  xsltSetDefaultSecurityPrefs(sec);
  exsltRegisterAll();

  strcpy(errbuf, "");

  xmlSubstituteEntitiesDefault(1);
  xmlLoadExtDtdDefaultValue = 1;
  exsltRegisterAll();

  while(running)
  {
    if (read(STDIN_FILENO, &rlen, 4) != 4)
      exit(1);
    rlen = ntohl(rlen);
    //fprintf(stderr, "Len: %i\n", rlen);
    if (rlen > rbuflen)
    {
      if (rbuf)
        free(rbuf);
      rbuf = malloc(rlen);
    }
    if (read(STDIN_FILENO, rbuf, rlen) != rlen)
      exit(1);

    switch(rbuf[0])
    {
    case CMD_SET_XSLT:
      //fprintf(stderr, "CMD_SET_XSLT\n");
      if (xslt)
      {
        xsltFreeStylesheet(xslt);
        xslt = NULL;
      }
      baseuri = rbuf + 1;
      baseurilen = strlen(baseuri);
      xsltDoc = xmlReadMemory(baseuri + baseurilen + 1, rlen - baseurilen - 2, baseuri, NULL, 0);
      if (xsltDoc)
      {
        xslt = xsltParseStylesheetDoc(xsltDoc);
        //xmlFreeDoc(xsltDoc);
      }
      break;
    case CMD_SET_XML:
      //fprintf(stderr, "CMD_SET_XML\n");
      if (xml)
      {
        xmlFreeDoc(xml);
        xml = NULL;
      }
      baseuri = rbuf + 1;
      baseurilen = strlen(baseuri);
      xml = xmlReadMemory(baseuri + baseurilen + 1, rlen - baseurilen - 2, baseuri, NULL, 0);
      break;
    case CMD_SET_PARAMS:
      params = parse_params(rbuf + 1, rlen - 1);
      break;
    case CMD_PROCESS:
      res = xsltApplyStylesheet(xslt, xml, (const char **)params);
      if (res)
      {
        char *mediaType = (xslt->mediaType ? xslt->mediaType : "");
        xsltSaveResultToString(&xmlbuf, &xmlbufsize, res, xslt);
        xmlFreeDoc(res);

        send_length(1 + strlen(mediaType) + 1 + xmlbufsize);
        send_byte(RPL_RESULT);
        write(STDOUT_FILENO, mediaType, strlen(mediaType) + 1);
        write(STDOUT_FILENO, xmlbuf, xmlbufsize);
        xmlFree(xmlbuf);
      }
      else
      {
        send_length(1 + strlen(errbuf));
        send_byte(RPL_ERROR);
        write(STDOUT_FILENO, errbuf, strlen(errbuf));
      }
      strcpy(errbuf, "");
      break;
    case CMD_REGISTER:
      xmlns = rbuf + 1;
      name = xmlns + strlen(xmlns) + 1;
      xsltRegisterExtModuleFunction((xmlChar *)name, (xmlChar *)xmlns,
                                    xmlXPathFuncCallback);
      break;
    }
  }

  xsltCleanupGlobals();
  xmlCleanupParser();
  return 0;
}
