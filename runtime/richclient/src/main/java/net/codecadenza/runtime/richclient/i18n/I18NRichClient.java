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
package net.codecadenza.runtime.richclient.i18n;

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
public class I18NRichClient {
	public static final String DATA_DOWNLOAD_THREAD_ERR_FILE_CREATION = "data_download_thread.err_file_creation";
	public static final String DATA_DOWNLOAD_THREAD_ERR_NO_FILE = "data_download_thread.err_no_file";
	public static final String DATA_DOWNLOAD_THREAD_ERR_NO_HTTP_CON = "data_download_thread.err_no_http_con";
	public static final String DATA_UPLOAD_THREAD_ERR_FILE_NOT_EXISTS = "data_upload_thread.err_file_not_exists";
	public static final String DATA_UPLOAD_THREAD_ERR_NO_FILE = "data_upload_thread.err_no_file";
	public static final String DATA_UPLOAD_THREAD_ERR_NO_HTTP_CON = "data_upload_thread.err_no_http_con";
	public static final String DESKTOP_HELPER_ERR_ILLEGAL_ARGUMENT = "desktop_helper.err_illegal_argument";
	public static final String DESKTOP_HELPER_ERR_OS_NOT_SUPPORTED = "desktop_helper.err_os_not_supported";
	public static final String FILE_TRANSPORT_HANDLER_ERR_FILE_NULL = "file_transport_handler.err_file_null";
	public static final String FILE_TRANSPORT_HANDLER_ERR_MISSING_LOCAL_PATH = "file_transport_handler.err_missing_local_path";
	public static final String FILE_TRANSPORT_HANDLER_ERR_MISSING_SERVER_PATH = "file_transport_handler.err_missing_server_path";
	public static final String FILE_TRANSPORT_HANDLER_ERR_NON_EXISTING_FILE = "file_transport_handler.err_non_existing_file";
	public static final String FILE_TRANSPORT_HANDLER_ERR_NOT_A_FILE = "file_transport_handler.err_not_a_file";
	public static final String SERVICE_LOCATOR_ERR_NOT_INITIALIZED = "service_locator.err_not_initialized";
	public static final String SERVICE_LOCATOR_ERR_NULL_PARAM = "service_locator.err_null_param";
	public static final String LOCAL_INVOCATION_HANDLER_IMP_NOT_FOUND = "local_invocation_handler.imp_not_found";
	public static final String LABEL_SEPARATOR = ":";
	private static final String BUNDLE_NAME = "i18n.richclient-translation";
	private static final ResourceBundle bundle = ResourceBundle.getBundle(BUNDLE_NAME, Locale.getDefault());

	/**
	 * Prevent instantiation
	 */
	private I18NRichClient() {

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
