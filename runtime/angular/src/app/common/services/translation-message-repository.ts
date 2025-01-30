import { Translation } from '../model/translation.interface';

/**
 * This class contains all translations that are used in TypeScript files
 */
export class TranslationMessageRepository {
  static readonly LOCALE_EN = 'EN';

  // Translation messages for the default locale
  private static readonly messagesEN: Array<Translation> =
    [
      { 'key': 'errordialog_title', 'message': 'An internal error has occurred!' },
      { 'key': 'dlg_header_conf', 'message': 'Confirmation required' },
      { 'key': 'msg_countresult', 'message': 'This query would return $(result) items!' },
      { 'key': 'msg_confirmcopy', 'message': 'Do you really want to create a copy of the selected object?' },
      { 'key': 'msg_confirmdelete', 'message': 'Do you really want to delete the selected object?' },
      { 'key': 'msg_errordataload', 'message': 'Error while loading data!' },
      { 'key': 'msg_finisheddataload', 'message': 'The query returned $(result) item(s)' },
      { 'key': 'msg_errorcount', 'message': 'Error while performing count operation!' },
      { 'key': 'msg_errorlogin', 'message': 'Login has failed! Please provide valid credentials!' },
      { 'key': 'msg_passwordmatch', 'message': 'The passwords do not match!' },
      { 'key': 'msg_minpasswordlength', 'message': 'The password is too short! Please enter $(length) characters at least!' },
      { 'key': 'msg_noconnection', 'message': 'No Internet connection!' },
      { 'key': 'msg_notauthorized', 'message': 'You are not authorized to access the requested resource!' },
      { 'key': 'msg_authenticationrequired', 'message': 'Authentication required!' },
      { 'key': 'msg_resourcenotfound', 'message': 'The requested resource does not exist!' },
      { 'key': 'msg_internalservererror', 'message': 'The server was not able to process the request!' },
      { 'key': 'msg_missingidparameter', 'message': 'Cannot initialize the page if the ID parameter is missing!'},
      { 'key': 'msg_noobjectpermission', 'message': 'You do not have the permission to load the selected object!' },
      { 'key': 'msg_treeviewstatustext', 'message': 'Number of loaded items: $(size)' },
      { 'key': 'msg_statusloading', 'message': 'Loading...' },
      { 'key': 'multiselectionlist_sourceheader', 'message': 'Available items ($(length))' },
      { 'key': 'multiselectionlist_targetheader', 'message': 'Selected items ($(length))' },
      { 'key': 'validation_required', 'message': 'Field must not be empty!' },
      { 'key': 'validation_minlength', 'message': 'The field must contain $(length) character(s) at least!' },
      { 'key': 'validation_maxlength', 'message': 'The field must contain $(length) character(s) at most!' },
      { 'key': 'validation_minvalue', 'message': 'The value must be greater than $(value)!' },
      { 'key': 'validation_maxvalue', 'message': 'The value must be smaller than $(value)!' },
      { 'key': 'validation_pattern', 'message': 'The value does not match the pattern $(pattern)!' },
      { 'key': 'validation_integer', 'message': 'The value is no valid integer!' },
      { 'key': 'validation_decimal', 'message': 'The value is no valid decimal!' },
      { 'key': 'msg_errorconvertdecimal', 'message': 'The value "$(value)" could not be converted to a decimal!' },
      { 'key': 'msg_errorconvertinteger', 'message': 'The value "$(value)" could not be converted to an integer!' },
      { 'key': 'msg_errorbetwen', 'message': 'The between operator requires two operands delimited by two whitespace characters!' }
    ];

  /**
   * Return the translation messages for a given locale. It will return
   * an empty array if the locale is not supported!
   */
  public static getTranlationMessages(locale: string) {
    if (locale === TranslationMessageRepository.LOCALE_EN) {
      return TranslationMessageRepository.messagesEN;
    }

    console.warn('No messages found for locale ' + locale + '!');
    return [];
  }

}
