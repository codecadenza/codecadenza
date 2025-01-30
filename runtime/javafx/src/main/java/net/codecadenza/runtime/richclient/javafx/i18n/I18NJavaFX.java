/*
 * This file is part of CodeCadenza, a set of tools, libraries and plug-ins
 * for modeling and creating Java-based enterprise applications.
 * For more information visit:
 *
 * https://github.com/codecadenza/
 *
 * This software is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this software; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
 */
package net.codecadenza.runtime.richclient.javafx.i18n;

import java.util.Locale;
import java.util.ResourceBundle;
import net.codecadenza.runtime.i18n.I18N;

/**
 * <p>
 * Utility class for the translation of text fragments within this library
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class I18NJavaFX {
	public static final String ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_NEW_PASSWORD = "abstract_change_password_dialog.lbl_new_password";
	public static final String ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_OLD_PASSWORD = "abstract_change_password_dialog.lbl_old_password";
	public static final String ABSTRACT_CHANGE_PASSWORD_DIALOG_LBL_PASSWORD_CONFIRM = "abstract_change_password_dialog.lbl_password_confirm";
	public static final String ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE = "abstract_change_password_dialog.title";
	public static final String ABSTRACT_CHANGE_PASSWORD_DIALOG_TITLE_MESSAGE = "abstract_change_password_dialog.title_message";
	public static final String ABSTRACT_LOG_ON_DIALOG_LBL_HOST = "abstract_log_on_dialog.lbl_host";
	public static final String ABSTRACT_LOG_ON_DIALOG_LBL_PASSWORD = "abstract_log_on_dialog.lbl_password";
	public static final String ABSTRACT_LOG_ON_DIALOG_LBL_USER_NAME = "abstract_log_on_dialog.lbl_user_name";
	public static final String ABSTRACT_LOG_ON_DIALOG_MSG_CONNECT_TO_HOST = "abstract_log_on_dialog.msg_connect_to_host";
	public static final String ABSTRACT_LOG_ON_DIALOG_MSG_INVALID_CREDENTIALS = "abstract_log_on_dialog.msg_invalid_credentials";
	public static final String ABSTRACT_LOG_ON_DIALOG_MSG_MISSING_USER = "abstract_log_on_dialog.msg_missing_user";
	public static final String ABSTRACT_LOG_ON_DIALOG_TITLE = "abstract_log_on_dialog.title";
	public static final String ABSTRACT_LOG_ON_DIALOG_TITLE_MESSAGE = "abstract_log_on_dialog.title_message";
	public static final String ABSTRACT_LOV_DIALOG_INFO_MESSAGE = "abstract_lov_dialog.info_message";
	public static final String ABSTRACT_LOV_DIALOG_LBL_FILTER = "abstract_lov_dialog.lbl_filter";
	public static final String ABSTRACT_LOV_DIALOG_SELECT = "abstract_lov_dialog.select";
	public static final String ABSTRACT_LOV_DIALOG_TITLE_MESSAGE = "abstract_lov_dialog.title_message";
	public static final String ABSTRACT_LOV_FIELD_CMD_OPEN = "abstract_lov_field.cmd_open";
	public static final String ABSTRACT_VIEW_NAVIGATOR_MSG_DELETE_QUERY = "abstract_view_navigator.msg_delete_query";
	public static final String ABSTRACT_VIEW_NAVIGATOR_MSG_DELETE_QUERY_TITLE = "abstract_view_navigator.msg_delete_query_title";
	public static final String ABSTRACT_VIEW_NAVIGATOR_ROOT_ITEM_LABEL = "abstract_view_navigator.root_item_label";
	public static final String ABSTRACT_VIEW_NAVIGATOR_SAVED_QUERIES_LABEL = "abstract_view_navigator.saved_queries_label";
	public static final String ACTION_COPY_TITLE = "action_copy.title";
	public static final String ACTION_DELETE_TITLE = "action_delete.title";
	public static final String ACTION_EXPORT_DIALOG_TITLE = "action_export.dialog_title";
	public static final String ACTION_EXPORT_MSG_EXPORT_ERROR = "action_export.msg_export_error";
	public static final String ACTION_EXPORT_TITLE = "action_export.title";
	public static final String ACTION_EXPORT_XLSX_FILTER_DESC = "action_export.xlsx_filter_desc";
	public static final String ACTION_FETCH_NEXT_TITLE = "action_fetch_next.title";
	public static final String ACTION_FETCH_PREV_TITLE = "action_fetch_prev.title";
	public static final String ACTION_REFRESH_TITLE = "action_refresh.title";
	public static final String ACTION_SAVE_QUERY_INPUT_DIALOG_NEW_QUERY_DEFAULT_VALUE = "action_save_query.input_dialog_new_query_default_value";
	public static final String ACTION_SAVE_QUERY_INPUT_DIALOG_NEW_QUERY_TITLE = "action_save_query.input_dialog_new_query_title";
	public static final String ACTION_SAVE_QUERY_MSG_OVERWRITE_QUERY = "action_save_query.msg_overwrite_query";
	public static final String ACTION_SAVE_QUERY_MSG_QUERY_DUPLICATE = "action_save_query.msg_query_duplicate";
	public static final String ACTION_SAVE_QUERY_MSG_QUERY_SAVE_ERROR = "action_save_query.msg_query_save_error";
	public static final String ACTION_SAVE_QUERY_MSG_QUERY_SAVED = "action_save_query.msg_query_saved";
	public static final String ACTION_SAVE_QUERY_MSG_SAVE_QUERY_TITLE = "action_save_query.msg_save_query_title";
	public static final String ACTION_SAVE_QUERY_TITLE = "action_save_query.title";
	public static final String ACTION_SEARCH_TITLE = "action_search.title";
	public static final String ACTION_SUSPEND_TITLE = "action_suspend.title";
	public static final String CMD_CANCEL = "cmd_cancel";
	public static final String CMD_COUNT = "cmd_count";
	public static final String CMD_NO = "cmd_no";
	public static final String CMD_OK = "cmd_ok";
	public static final String CMD_RESET = "cmd_reset";
	public static final String CMD_YES = "cmd_yes";
	public static final String DATA_FETCH_ACTION_MSG_QUERY_FAILED = "data_fetch_action.msg_query_failed";
	public static final String DATA_FETCH_ACTION_RESULT_NO_COUNT = "data_fetch_action.result_no_count";
	public static final String DATA_FETCH_ACTION_RESULT_WITH_COUNT = "data_fetch_action.result_with_count";
	public static final String DATA_FETCH_ACTION_STATUS_FETCH_DATA = "data_fetch_action.status_fetch_data";
	public static final String DATA_FETCH_ACTION_STATUS_OP_CANCELED = "data_fetch_action.status_op_canceled";
	public static final String FILE_DOWNLOAD_DIALOG_BYTES_PROCESSED = "file_download_dialog.bytes_processed";
	public static final String FILE_DOWNLOAD_DIALOG_TITLE = "file_download_dialog.title";
	public static final String FILE_UPLOAD_DIALOG_BYTES_PROCESSED = "file_upload_dialog.bytes_processed";
	public static final String FILE_UPLOAD_DIALOG_TITLE = "file_upload_dialog.title";
	public static final String FORMAT_PREFERENCES_DIALOG_LBL_DATE_FORMAT = "format_preferences_dialog.lbl_date_format";
	public static final String FORMAT_PREFERENCES_DIALOG_LBL_DATE_TIME_FORMAT = "format_preferences_dialog.lbl_date_time_format";
	public static final String FORMAT_PREFERENCES_DIALOG_LBL_NUMBER_FORMAT = "format_preferences_dialog.lbl_number_format";
	public static final String FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE = "format_preferences_dialog.msg_invalid_date";
	public static final String FORMAT_PREFERENCES_DIALOG_MSG_INVALID_DATE_TIME = "format_preferences_dialog.msg_invalid_date_time";
	public static final String FORMAT_PREFERENCES_DIALOG_MSG_INVALID_NUMBER = "format_preferences_dialog.msg_invalid_number";
	public static final String FORMAT_PREFERENCES_DIALOG_TITLE = "format_preferences_dialog.title";
	public static final String INPUT_DIALOG_LBL_INPUT = "input_dialog.lbl_input";
	public static final String INPUT_DIALOG_MSG_INPUT_REQUIRED = "input_dialog.msg_input_required";
	public static final String INPUT_DIALOG_MSG_INPUT_REQUIRED_TITLE = "input_dialog.msg_input_required_title";
	public static final String MESSAGE_DIALOG_LBL_DETAILS = "message_dialog.lbl_details";
	public static final String MSG_ERR_MIN_FIELD_LENGTH = "msg_err_min_field_length";
	public static final String MSG_ERR_PASSWORDS_NOT_EQUAL = "msg_err_passwords_not_equal";
	public static final String SEARCH_INPUT_DIALOG_CBO_EXACT_FILTER_MATCH = "search_input_dialog.cbo_exact_filter_match";
	public static final String SEARCH_INPUT_DIALOG_CHK_CASE_SENSITIVE = "search_input_dialog.chk_case_sensitive";
	public static final String SEARCH_INPUT_DIALOG_CHK_COUNT_RECORDS = "search_input_dialog.chk_count_records";
	public static final String SEARCH_INPUT_DIALOG_LBL_FIELD_NAME = "search_input_dialog.lbl_field_name";
	public static final String SEARCH_INPUT_DIALOG_LBL_FILTER = "search_input_dialog.lbl_filter";
	public static final String SEARCH_INPUT_DIALOG_LBL_OPERATOR = "search_input_dialog.lbl_operator";
	public static final String SEARCH_INPUT_DIALOG_LBL_SORT_ORDER = "search_input_dialog.lbl_sort_order";
	public static final String SEARCH_INPUT_DIALOG_LIST_COLUMNS_TITLE = "search_input_dialog.list_columns_title";
	public static final String SEARCH_INPUT_DIALOG_MAX_FETCH_SIZE = "search_input_dialog.max_fetch_size";
	public static final String SEARCH_INPUT_DIALOG_MSG_COUNT_ERROR = "search_input_dialog.msg_count_error";
	public static final String SEARCH_INPUT_DIALOG_MSG_COUNT_RES = "search_input_dialog.msg_count_res";
	public static final String SEARCH_INPUT_DIALOG_MSG_COUNT_RES_TITLE = "search_input_dialog.msg_count_res_title";
	public static final String SEARCH_INPUT_DIALOG_MSG_ERR_CRITERION_EXP = "search_input_dialog.msg_err_criterion_exp";
	public static final String SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_BETWEEN = "search_input_dialog.msg_err_missing_between";
	public static final String SEARCH_INPUT_DIALOG_MSG_ERR_MISSING_IN = "search_input_dialog.msg_err_missing_in";
	public static final String SEARCH_INPUT_DIALOG_MSG_ERR_NO_NUMBER = "search_input_dialog.msg_err_no_number";
	public static final String SEARCH_INPUT_DIALOG_MSG_ERR_NO_UUID = "search_input_dialog.msg_err_no_uuid";
	public static final String SEARCH_INPUT_DIALOG_TAB_ADV_SETTINGS = "search_input_dialog.tab_adv_settings";
	public static final String SEARCH_INPUT_DIALOG_TAB_FILTER = "search_input_dialog.tab_filter";
	public static final String SEARCH_INPUT_DIALOG_TITLE = "search_input_dialog.title";
	public static final String SEARCH_INPUT_DIALOG_TITLE_MSG = "search_input_dialog.title_msg";
	public static final String SELECTION_LIST_PANEL_LBL_SEARCH_ELEMENTS = "selection_list_panel.lbl_search_elements";
	public static final String SELECTION_LIST_PANEL_LBL_SOURCE_LIST = "selection_list_panel.lbl_source_list";
	public static final String SELECTION_LIST_PANEL_LBL_TARGET_LIST = "selection_list_panel.lbl_target_list";
	private static final String BUNDLE_NAME = "i18n.javafx-translation";
	private static final ResourceBundle bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());

	/**
	 * Prevent instantiation
	 */
	private I18NJavaFX() {

	}

	/**
	 * @param key
	 * @param params
	 * @return the translation text by using the given key
	 */
	public static String getTranslation(String key, Object... params) {
		return I18N.getTranslation(bundle, key, params);
	}

	/**
	 * @param key
	 * @return the translation for a field label by using the given key
	 */
	public static String getTranslationForFieldLabel(String key) {
		return I18N.getTranslationForFieldLabel(bundle, key);
	}

}
