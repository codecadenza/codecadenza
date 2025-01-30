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
package net.codecadenza.runtime.richclient.eclipse.widget;

import org.eclipse.swt.widgets.Composite;

/**
 * <p>
 * Simple implementation of a data grid composite
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid composite
 */
public class DataGridComposite<T> extends __DataGridComposite<T> {
	private static final long serialVersionUID = -3726284830198143329L;

	/**
	 * Constructor
	 * @param parent
	 * @param style
	 */
	public DataGridComposite(Composite parent, int style) {
		super(parent, style);

		initTableListeners();
		initPopUpMenu();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getSelectedCellContent()
	 */
	@Override
	public final String getSelectedCellContent() {
		return "";
	}

	/**
	 * Initialize default table listeners
	 */
	protected void initTableListeners() {
		// The implementation must be provided by a subclass!
	}

	/**
	 * Add the default pop-up menu items
	 */
	protected void initPopUpMenu() {
		// The implementation must be provided by a subclass!
	}

}
