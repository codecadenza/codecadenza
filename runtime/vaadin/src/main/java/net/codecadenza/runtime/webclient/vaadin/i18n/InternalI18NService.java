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
package net.codecadenza.runtime.webclient.vaadin.i18n;

import java.util.Locale;
import java.util.ResourceBundle;

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
public class InternalI18NService extends AbstractI18NService {
	private static final long serialVersionUID = 6018555612223358244L;
	public static final String ABSTRACT_DATA_GRID_ACTION_REFRESH = "abstract_data_grid.action_refresh";
	public static final String ABSTRACT_ELEMENT_COLLECTION_EDITOR_LBL_ADD = "abstract_element_collection_editor.lbl_add";
	public static final String ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_TITLE_CONVERSION = "abstract_element_collection_editor.msg_title_conversion";
	public static final String ABSTRACT_ELEMENT_COLLECTION_EDITOR_MSG_CONVERSION_FAILED = "abstract_element_collection_editor.msg_conversion_failed";
	public static final String DUAL_LIST_FIELD_LEFT_COL = "dual_list_field.list_left_col";
	public static final String DUAL_LIST_FIELD_RIGHT_COL = "dual_list_field.list_right_col";
	public static final String DUAL_FILTERING_LIST_FIELD_LBL_INPUT = "dual_filtering_list_field.lbl_input";
	public static final String CMD_ADD = "cmd_add";
	public static final String CMD_CANCEL = "cmd_cancel";
	public static final String CMD_COUNT = "cmd_count";
	public static final String CMD_DELETE = "cmd_delete";
	public static final String CMD_DELETE_ALL = "cmd_delete_all";
	public static final String CMD_EXPORT = "cmd_export";
	public static final String CMD_NO = "cmd_no";
	public static final String CMD_OK = "cmd_ok";
	public static final String CMD_REFRESH = "cmd_refresh";
	public static final String CMD_RESET = "cmd_reset";
	public static final String CMD_SET = "cmd_set";
	public static final String CMD_SEARCH = "cmd_search";
	public static final String CMD_YES = "cmd_yes";
	public static final String ERROR_MESSAGE_DIALOG_OUTPUT_CAUSE = "error_message_dialog.output_cause";
	public static final String ERROR_MESSAGE_DIALOG_OUTPUT_MESSAGE = "error_message_dialog.output_message";
	public static final String ERROR_MESSAGE_DIALOG_OUTPUT_STACK_TRACE = "error_message_dialog.output_stack_trace";
	public static final String FILE_UPLOAD_DIALOG_TITLE = "file_upload_dialog.title";
	public static final String INPUT_DIALOG_MSG_ERROR_NO_VALUE = "input_dialog.msg_error_no_value";
	public static final String ABSTRACT_LOV_DIALOG_LBL_INPUT = "abstract_lov_dialog.lbl_input";
	public static final String LOV_FIELD_CBOITEMLIST_INPUTPROMPT = "lov_field.cboitemlist_inputprompt";
	public static final String MSG_COUNT_ERROR = "msg_count_error";
	public static final String MSG_COUNT_RESULT = "msg_count_result";
	public static final String MSG_COUNT_TITLE = "msg_count_title";
	public static final String MSG_NUMBER_CONVERSION_ERROR = "msg_number_conversion_error";
	public static final String MSG_UUID_CONVERSION_ERROR = "msg_uuid_conversion_error";
	public static final String MSG_DATA_FETCH = "msg_data_fetch";
	public static final String MSG_DATA_FETCH_NO_COUNT = "msg_data_fetch_no_count";
	public static final String MSG_DATA_FETCH_NO_RESULTS = "msg_data_fetch_no_results";
	public static final String MSG_DATA_FETCH_WITH_COUNT = "msg_data_fetch_with_count";
	public static final String MSG_LOOKUP_ERROR = "msg_lookup_error";
	public static final String MSG_REFRESH_ERROR = "msg_refresh_error";
	public static final String MSG_SEARCH_ERROR = "msg_search_error";
	public static final String MSG_UNEXPECTED_ERROR_TITLE = "msg_unexpected_error_title";
	public static final String MSG_UPLOAD_ERROR = "msg_upload_error";
	public static final String MSG_UPLOAD_ILLEGAL_MIMETYPE = "msg_upload_illegal_mimetype";
	public static final String MSG_UPLOAD_TITLE = "msg_upload_title";
	public static final String MSG_ERR_ITEM_NOT_FOUND = "msg_err_item_not_found";
	public static final String MSG_ERR_FIELD_REGEX = "msg_err_field_regex";
	public static final String MSG_ERR_FIELD_LENGTH_RANGE = "msg_err_field_length_range";
	public static final String MSG_ERR_FIELD_MAX_LENGTH = "msg_err_field_max_length";
	public static final String MSG_ERR_FIELD_MIN_LENGTH = "msg_err_field_min_length";
	public static final String MSG_ERR_FIELD_MAX_VALUE = "msg_err_field_max_value";
	public static final String MSG_ERR_FIELD_MIN_VALUE = "msg_err_field_min_value";
	public static final String MSG_ERR_FIELD_VALUE_RANGE = "msg_err_field_value_range";
	public static final String MSG_VALIDATION_ERROR = "msg_validation_error";
	public static final String SEARCH_INPUT_DIALOG_CBO_FETCHSIZE = "search_input_dialog.cbo_fetchsize";
	public static final String SEARCH_INPUT_DIALOG_CHK_CASESENSITIVE = "search_input_dialog.chk_casesensitive";
	public static final String SEARCH_INPUT_DIALOG_CHK_COUNT = "search_input_dialog.chk_count";
	public static final String SEARCH_INPUT_DIALOG_CHK_EXACTFILTER = "search_input_dialog.chk_exactfilter";
	public static final String SEARCH_INPUT_DIALOG_TAB_PAGE_ADDSETTINGS = "search_input_dialog.tab_page_addsettings";
	public static final String SEARCH_INPUT_DIALOG_TAB_PAGE_FILTER = "search_input_dialog.tab_page_filter";
	public static final String SEARCH_INPUT_DIALOG_TITLE = "search_input_dialog.title";
	private static final String BUNDLE_NAME = "i18n.vaadin-translation";

	private final transient ResourceBundle bundle;

	/**
	 * Constructor
	 * @param locale
	 */
	public InternalI18NService(Locale locale) {
		super(locale);

		this.bundle = ResourceBundle.getBundle(BUNDLE_NAME, locale);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.i18n.AbstractI18NService#getBundle()
	 */
	@Override
	protected ResourceBundle getBundle() {
		return bundle;
	}

}
