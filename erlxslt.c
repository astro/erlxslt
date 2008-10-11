#include <stdint.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <string.h>
#include <libxml/parser.h>
#include <libxml/tree.h>
#include <libxslt/xslt.h>
#include <libxslt/transform.h>
#include <libexslt/exslt.h>
#include <libxslt/extensions.h>
#include <ei.h>
#include <libxml/xpathInternals.h>


#define CMD_SET_XSLT 1
#define CMD_SET_XML 2
#define CMD_PROCESS 3
#define CMD_REGISTER_EXT 4

void send_length(uint32_t len)
{
  len = htonl(len);
  write(STDOUT_FILENO, &len, 4);
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
  fprintf(stderr, "callback %i\n",nargs);

  uint32_t rlen;
  int rindex = 0, version;
  char *rbuf = NULL;
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
  write(STDOUT_FILENO, "\1", 1);
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
    ret = xmlXPathWrapString(xmlStrdup(term.value.atom_name));
    break;
    /* TODO */
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
  int xmlbufsize;
  const char *params[] = {NULL};
  char *name, *xmlns;

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
      xsltDoc = xmlReadMemory(rbuf + 1, rlen - 1, "", NULL, 0);
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
      //fprintf(stderr, "xmlReadMemory\n");
      xml = xmlReadMemory(rbuf + 1, rlen - 1, "", NULL, 0);
      //fprintf(stderr, "done\n");
      break;
    case CMD_PROCESS:
      //fprintf(stderr, "CMD_PROCESS (%X, %X)\n", xslt, xml);
      res = xsltApplyStylesheet(xslt, xml, params);
      //fprintf(stderr, "CMD_PROCESS formatting\n");
      xmlDocDumpFormatMemory(res, &xmlbuf, &xmlbufsize, 1);
      xmlFreeDoc(res);

      //fprintf(stderr, "CMD_PROCESS sending\n");
      send_length(xmlbufsize + 1);
      write(STDOUT_FILENO, "\0", 1);
      write(STDOUT_FILENO, xmlbuf, xmlbufsize);
      //fprintf(stderr, "CMD_PROCESS sent\n");

      /*xmlMemFree(xmlbuf);
      xmlbuf = NULL;*/
      break;
    case CMD_REGISTER_EXT:
      xmlns = rbuf + 1;
      name = xmlns + strlen(xmlns) + 1;
      fprintf(stderr, "register %s %s\n", name, xmlns);
      xsltRegisterExtModuleFunction((xmlChar *)name, (xmlChar *)xmlns,
                                    xmlXPathFuncCallback);
      break;
    }
  }

  xsltCleanupGlobals();
  xmlCleanupParser();
  return 0;
}
