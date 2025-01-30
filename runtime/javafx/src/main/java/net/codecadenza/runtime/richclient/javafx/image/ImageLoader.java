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
package net.codecadenza.runtime.richclient.javafx.image;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import javafx.scene.image.Image;

/**
 * <p>
 * Image loader
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ImageLoader {
	private static Map<String, Image> imageRegistry = Collections.synchronizedMap(new HashMap<>());
	public static final String IMAGE_DEFAULT_PATH = "/img/";

	public static final String IMG_ADD_DATA_TITLE = "add_data_title.png";
	public static final String IMG_EDIT_DATA_TITLE = "edit_data_title.png";
	public static final String IMG_NEW_DATA_TITLE = "new_data_title.png";
	public static final String IMG_VIEW_DATA_TITLE = "view_data_title.png";
	public static final String IMG_LOV_TITLE = "window_lov_title.png";
	public static final String IMG_FORMAT_TITLE = "format_preferences.png";
	public static final String IMG_DEFAULT_TITLE_IMAGE = "edit_data_title.png";
	public static final String IMG_SEARCH_TITLE = "window_search_title.png";
	public static final String IMG_DIALOG_INFO = "dialog_information.png";
	public static final String IMG_DIALOG_WARNING = "dialog_warning.png";
	public static final String IMG_DIALOG_ERROR = "dialog_error.png";
	public static final String IMG_DIALOG_CONFIRM = "dialog_confirm.png";
	public static final String IMG_CALENDAR = "calendar.png";
	public static final String IMG_CHECKED = "checked.gif";
	public static final String IMG_UNCHECKED = "unchecked.gif";
	public static final String IMG_DELETE = "delete.png";
	public static final String IMG_DESELECT_ITEM = "deselect_item.png";
	public static final String IMG_DESELECT_ALL = "deselect_all.png";
	public static final String IMG_DOWNLOAD = "download.png";
	public static final String IMG_EDIT_DATA = "edit_data.png";
	public static final String IMG_ERROR = "error.png";
	public static final String IMG_ERROR_TITLE = "error_title.png";
	public static final String IMG_EXPORT_EXCEL = "export_excel.png";
	public static final String IMG_FOLDER = "folder.png";
	public static final String IMG_HOST = "host.png";
	public static final String IMG_INFO = "information.png";
	public static final String IMG_LOGON = "logon.png";
	public static final String IMG_LOV = "lov.png";
	public static final String IMG_NAVIGATOR = "tree_navigator.png";
	public static final String IMG_NEW_DATA = "new_data.png";
	public static final String IMG_PREFERENCES = "preferences.png";
	public static final String IMG_REFRESH = "refresh.png";
	public static final String IMG_SAVE_AS = "save_as.png";
	public static final String IMG_SAVED_SEARCH = "saved_search.png";
	public static final String IMG_SEARCH = "search.png";
	public static final String IMG_SELECT_ITEM = "select_item.png";
	public static final String IMG_SELECT_ALL = "select_all.png";
	public static final String IMG_SORT_ASC = "sort_ascending.png";
	public static final String IMG_SORT_DESC = "sort_descending.png";
	public static final String IMG_STOP = "stop.png";
	public static final String IMG_SUB_ITEM = "sub_item.png";
	public static final String IMG_SUB_ITEMS = "sub_items.png";
	public static final String IMG_SWITCH_USER = "switch_user.png";
	public static final String IMG_TREE_DATA = "tree_data.png";
	public static final String IMG_UPLOAD = "upload.png";
	public static final String IMG_VIEW = "view.png";
	public static final String IMG_VIEW_DATA = "view_data.png";
	public static final String IMG_VIEW_NEXT = "view_next.png";
	public static final String IMG_VIEW_PREV = "view_previous.png";
	public static final String IMG_COPY = "copy.png";
	public static final String IMG_DATA_IMPORT = "data_import.png";
	public static final String IMG_DATA_EXPORT = "data_export.png";

	/**
	 * Prevent instantiation
	 */
	private ImageLoader() {

	}

	/**
	 * @param name the unique name of the image
	 * @return the image or null if the image could not be found
	 */
	public static Image getImage(String name) {
		return imageRegistry.computeIfAbsent(name, key -> new Image(IMAGE_DEFAULT_PATH + key, true));
	}

}
