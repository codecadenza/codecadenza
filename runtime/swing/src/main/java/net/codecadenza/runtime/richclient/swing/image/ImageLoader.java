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
package net.codecadenza.runtime.richclient.swing.image;

import java.net.URL;
import javax.swing.ImageIcon;

/**
 * <p>
 * Utility to load images
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ImageLoader {
	private static final String RESOURCE_PATH = "/img/";

	// Title area images
	public static final String ADD_DATA_TITLE = "add_data_title.png";
	public static final String EDIT_DATA_TITLE = "edit_data_title.png";
	public static final String NEW_DATA_TITLE = "new_data_title.png";
	public static final String VIEW_DATA_TITLE = "view_data_title.png";
	public static final String LOV_TITLE = "window_lov_title.png";
	public static final String FORMAT_TITLE = "format_preferences.png";
	public static final String DEFAULT_TITLE_IMAGE = "edit_data_title.png";
	public static final String SEARCH_TITLE = "window_search_title.png";

	public static final String CALENDAR = "calendar.png";
	public static final String CHECKED = "checked.gif";
	public static final String UNCHECKED = "unchecked.gif";
	public static final String DELETE = "delete.png";
	public static final String DESELECT_ITEM = "deselect_item.png";
	public static final String DESELECT_ALL = "deselect_all.png";
	public static final String DOWNLOAD = "download.png";
	public static final String EDIT_DATA = "edit_data.png";
	public static final String ERROR = "error.png";
	public static final String EXPORT_EXCEL = "export_excel.png";
	public static final String FOLDER = "folder.png";
	public static final String HOST = "host.png";
	public static final String INFO = "information.png";
	public static final String LOGON = "logon.png";
	public static final String LOV = "lov.png";
	public static final String NAVIGATOR = "tree_navigator.png";
	public static final String NEW_DATA = "new_data.png";
	public static final String PREFERENCES = "preferences.png";
	public static final String REFRESH = "refresh.png";
	public static final String SAVE_AS = "save_as.png";
	public static final String SAVED_SEARCH = "saved_search.png";
	public static final String SEARCH = "search.png";
	public static final String SELECT_ITEM = "select_item.png";
	public static final String SELECT_ALL = "select_all.png";
	public static final String SORT_ASC = "sort_ascending.png";
	public static final String SORT_DESC = "sort_descending.png";
	public static final String STOP = "stop.png";
	public static final String SUB_ITEM = "sub_item.png";
	public static final String SUB_ITEMS = "sub_items.png";
	public static final String SWITCH_USER = "switch_user.png";
	public static final String TREE_DATA = "tree_data.png";
	public static final String UPLOAD = "upload.png";
	public static final String VIEW = "view.png";
	public static final String VIEW_DATA = "view_data.png";
	public static final String VIEW_NEXT = "view_next.png";
	public static final String VIEW_PREV = "view_previous.png";
	public static final String COPY = "copy.png";
	public static final String DATA_IMPORT = "data_import.png";
	public static final String DATA_EXPORT = "data_export.png";

	/**
	 * Prevent instantiation
	 */
	private ImageLoader() {

	}

	/**
	 * @param name
	 * @return the image icon
	 */
	public static ImageIcon getImage(String name) {
		if (name == null || name.isEmpty())
			return null;

		final URL resource = ImageLoader.class.getResource(RESOURCE_PATH + name);

		if (resource != null)
			return new ImageIcon(resource);

		return null;
	}

}
