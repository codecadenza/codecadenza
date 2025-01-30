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
package net.codecadenza.runtime.richclient.swing.widget;

/**
 * <p>
 * Helper class to store basic column information
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ColumnInfo {
	private final int index;
	private final boolean visible;
	private final int visibleIndex;
	private final String title;
	private int width;

	/**
	 * Constructor
	 * @param index
	 * @param visible
	 * @param visibleIndex
	 * @param title
	 */
	public ColumnInfo(int index, boolean visible, int visibleIndex, String title) {
		this.index = index;
		this.visible = visible;
		this.visibleIndex = visibleIndex;
		this.title = title;
	}

	/**
	 * Constructor
	 * @param index
	 * @param title
	 * @param width
	 */
	public ColumnInfo(int index, String title, int width) {
		this.index = index;
		this.visible = true;
		this.visibleIndex = index;
		this.title = title;
		this.width = width;
	}

	/**
	 * @return the index
	 */
	public int getIndex() {
		return index;
	}

	/**
	 * @return true if the column is visible
	 */
	public boolean isVisible() {
		return visible;
	}

	/**
	 * @return the visual index
	 */
	public int getVisibleIndex() {
		return visibleIndex;
	}

	/**
	 * @return the title
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * @return the column's default width
	 */
	public int getWidth() {
		return width;
	}

}
