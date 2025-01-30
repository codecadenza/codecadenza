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
package net.codecadenza.runtime.richclient.eclipse.image;

import java.util.HashMap;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;

/**
 * <p>
 * Image cache
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ImageCache {
	private static HashMap<String, Image> imageRegistry = new HashMap<>();
	public static final String IMAGE_DEFAULT_PATH = "/img/";

	public static final String IMG_ADD_DATE_DLG = "add_data_title.png";
	public static final String IMG_EDIT_DATA_DLG = "edit_data_title.png";
	public static final String IMG_EDIT_DATA = "edit_data.png";
	public static final String IMG_FORMAT_PREFERENCES = "format_preferences.png";
	public static final String IMG_LOGON = "logon.png";
	public static final String IMG_NAVIGATOR = "tree_navigator.png";
	public static final String IMG_NEW_DATA_DLG = "new_data_title.png";
	public static final String IMG_NEW_DATA = "new_data.png";
	public static final String IMG_PREFERENCES = "preferences.png";
	public static final String IMG_SEARCH_LIST = "search_list.png";
	public static final String IMG_SELECT = "select.png";
	public static final String IMG_DESELECT = "deselect.png";
	public static final String IMG_VALIDATION_ERROR = "validation_error.png";
	public static final String IMG_VIEW_DATA_DLG = "view_data_title.png";
	public static final String IMG_VIEW_DATA = "view_data.png";
	public static final String IMG_WINDOW_SAVED_SEARCH = "window_saved_search.png";
	public static final String IMG_WINDOW_VIEW = "window_view.png";
	public static final String IMG_EXCEL = "excel.png";
	public static final String IMG_LOV = "lov.png";
	public static final String IMG_REFRESH = "refresh.gif";
	public static final String IMG_SAVE_AS = "save_as.png";
	public static final String IMG_SEARCH = "search.png";
	public static final String IMG_UNCHECKED = "unchecked.gif";
	public static final String IMG_CHECKED = "checked.gif";
	public static final String IMG_VIEW_NEXT = "view_next.png";
	public static final String IMG_VIEW_PREVIOUS = "view_previous.png";
	public static final String IMG_TITLE_LOV = "window_lov_title.png";
	public static final String IMG_TITLE_SEARCH = "window_search_title.png";
	public static final String IMG_STOP_PROCESS = "stop.png";
	public static final String IMG_PERFORM_FETCH = "perform_fetch.png";
	public static final String IMG_SORT_ASC = "sort_ascending.png";
	public static final String IMG_SORT_DESC = "sort_descending.png";
	public static final String IMG_MOVE_UP = "move_up.png";
	public static final String IMG_MOVE_DOWN = "move_down.png";
	public static final String IMG_CALENDAR = "calendar.png";
	public static final String IMG_TREE_FOLDER = "folder.png";
	public static final String IMG_TREE_DATA = "tree_data.png";
	public static final String IMG_TREE_ITEM = "sub_item.png";
	public static final String IMG_ITEM = "sub_item.png";
	public static final String IMG_TREE_ITEMS = "sub_items.png";
	public static final String IMG_SWITCH_USER = "switch_user.png";
	public static final String IMG_HOST = "host.png";
	public static final String IMG_ERROR = "error.png";
	public static final String IMG_INFO = "information.png";
	public static final String IMG_COPY = "copy.png";
	public static final String IMG_DELETE = "delete.png";

	/**
	 * Prevent instantiation
	 */
	private ImageCache() {

	}

	/**
	 * Get an image from the cache
	 * @param name the unique name of the image
	 * @return the image
	 */
	public static synchronized Image getImage(String name) {
		return imageRegistry.computeIfAbsent(name,
				key -> ImageDescriptor.createFromFile(ImageCache.class, IMAGE_DEFAULT_PATH + key).createImage());
	}

	/**
	 * Get an image descriptor
	 * @param name the unique name of the image
	 * @return the image descriptor
	 */
	public static synchronized ImageDescriptor getImageDescriptor(String name) {
		return ImageDescriptor.createFromFile(ImageCache.class, IMAGE_DEFAULT_PATH + name);
	}

}
