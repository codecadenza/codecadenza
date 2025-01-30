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
package net.codecadenza.runtime.webclient.vaadin.component;

import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;

/**
 * <p>
 * Abstract data grid panel
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid
 */
public abstract class AbstractDataGridPanel<T> extends AbstractDataGrid<T> {
	private static final long serialVersionUID = -4711215247194772319L;

	protected boolean readOnly;

	/**
	 * Constructor
	 * @param i18n
	 * @param preferences
	 */
	protected AbstractDataGridPanel(I18NService i18n, PreferencesStore preferences) {
		super(i18n, preferences);

		setMargin(false);
		setPadding(false);
		setAddButtonBar(true);
	}

	/**
	 * @return true if the component should disable actions that create, update or delete data
	 */
	public boolean isReadOnly() {
		return readOnly;
	}

	/**
	 * @param readOnly
	 */
	public void setReadOnly(boolean readOnly) {
		this.readOnly = readOnly;
	}

}
