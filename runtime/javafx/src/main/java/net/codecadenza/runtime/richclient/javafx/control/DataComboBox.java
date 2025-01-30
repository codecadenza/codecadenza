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
package net.codecadenza.runtime.richclient.javafx.control;

import javafx.scene.control.ComboBox;
import javafx.util.StringConverter;

/**
 * <p>
 * Abstract generic base class for read-only comboboxes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the combobox
 */
public abstract class DataComboBox<T> extends ComboBox<T> {
	/**
	 * @param element
	 * @return the string representation of a given element
	 */
	public abstract String getItemText(T element);

	/**
	 * Constructor
	 */
	protected DataComboBox() {
		setEditable(false);

		setConverter(new StringConverter<T>() {
			/*
			 * (non-Javadoc)
			 * @see javafx.util.StringConverter#fromString(java.lang.String)
			 */
			@Override
			public T fromString(String arg0) {
				// Not required for a non-editable combobox
				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see javafx.util.StringConverter#toString(java.lang.Object)
			 */
			@Override
			public String toString(T arg0) {
				return getItemText(arg0);
			}
		});
	}

}
