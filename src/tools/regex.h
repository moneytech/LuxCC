#ifndef REGEX_H_
#define REGEX_H_

/*
 * regex_match: search for regex anywhere in text.
 * Return value:
 *  -  1: match
 *  -  0: no match
 *  - -1: error (call regex_get_error to get more info)
 */
int regex_match(char *regex, char *text);

/*
 * regex_match2: search for regex anywhere in text.
 * If there is a match, start and end are set to point
 * to the beginning and end of the matched text.
 * Otherwise they are left unmodified.
 * Return value: same as regex_match.
 */
int regex_match2(char *regex, char *text, char **start, char **end);

/*
 * regex_get_error: get a string that describes the most recent error.
 * This function should only be called after regex_match or regex_match2 returned -1.
 * The returned string must not be modified by the application.
 */
const char *regex_get_error(void);

#endif
