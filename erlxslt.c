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

#define CMD_SET_XSLT 1
#define CMD_SET_XML 2
#define CMD_PROCESS 3
#define CMD_SET_PARAMS 6
#define RPL_RESULT 4
#define RPL_ERROR 5

/*void send_string(const char *s)
{
  uint32_t len = htonl(strlen(s));
  write(STDOUT_FILENO, &len, 4);
  write(STDOUT_FILENO, s, strlen(s));
  }*/

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
      n++;
    }
  params[n - 1] = NULL;

  return params;
}

void send_length(uint32_t len)
{
  uint32_t l = htonl(len);
  write(STDOUT_FILENO, &l, 4);
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


extern int xmlLoadExtDtdDefaultValue;

int main()
{
  uint32_t rlen, running = 1, rbuflen = 0;
  char *rbuf = NULL;
  char sbuf[16];
  xsltStylesheetPtr xslt = NULL;
  xmlDocPtr xml = NULL, xsltDoc, res;
  xmlChar *xmlbuf = NULL;
  xsltSecurityPrefsPtr sec;
  int xmlbufsize;
  const char *default_params[] = {NULL};
  char **params = (char **)default_params;
  char *baseuri;
  int baseurilen;

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
        xsltSaveResultToString(&xmlbuf, &xmlbufsize, res, xslt);
        xmlFreeDoc(res);

        send_length(xmlbufsize + 1 + strlen(xslt->mediaType) + 1);
        sbuf[0] = RPL_RESULT;
        write(STDOUT_FILENO, sbuf, 1);
        write(STDOUT_FILENO, xslt->mediaType, strlen(xslt->mediaType) + 1);
        write(STDOUT_FILENO, xmlbuf, xmlbufsize);
        xmlFree(xmlbuf);
      }
      else
      {
        send_length(1 + strlen(errbuf));
        sbuf[0] = RPL_ERROR;
        write(STDOUT_FILENO, sbuf, 1);
        write(STDOUT_FILENO, errbuf, strlen(errbuf));
      }
      strcpy(errbuf, "");
      break;
    }
  }

  xsltCleanupGlobals();
  xmlCleanupParser();
  return 0;
}
