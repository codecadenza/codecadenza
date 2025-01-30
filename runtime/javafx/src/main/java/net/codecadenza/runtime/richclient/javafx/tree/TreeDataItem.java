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
package net.codecadenza.runtime.richclient.javafx.tree;

import javafx.scene.control.TreeItem;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

/**
 * <p>
 * Tree item that provides a configurable text
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class TreeDataItem extends TreeItem<String> {
	private final Object data;
	private final Image image;
	private boolean dataLoaded;
	private String groupName;

	/**
	 * Constructor
	 * @param data
	 * @param label
	 * @param image
	 */
	public TreeDataItem(Object data, String label, Image image) {
		super(label);

		this.setGraphic(new ImageView(image));
		this.data = data;
		this.image = image;
	}

	/**
	 * Constructor
	 * @param label
	 * @param image
	 */
	public TreeDataItem(String label, Image image) {
		this(null, label, image);
	}

	/**
	 * @return the contained data object
	 */
	public Object getData() {
		return data;
	}

	/**
	 * @return a copy of this tree item
	 */
	public TreeDataItem copy() {
		return new TreeDataItem(data, this.getValue(), image);
	}

	/**
	 * @return true if the data for this item has been loaded
	 */
	public boolean isDataLoaded() {
		return dataLoaded;
	}

	/**
	 * @param dataLoaded
	 */
	public void setDataLoaded(boolean dataLoaded) {
		this.dataLoaded = dataLoaded;
	}

	/**
	 * @return the item's group name
	 */
	public String getGroupName() {
		return groupName;
	}

	/**
	 * @param groupName
	 */
	public void setGroupName(String groupName) {
		this.groupName = groupName;
	}

}
