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
package net.codecadenza.runtime.richclient.swing.search.input.field;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.util.Map;
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;
import org.jdesktop.swingx.autocomplete.AutoCompleteDecorator;

/**
 * <p>
 * Input field for enumeration types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EnumField extends InputField {
	private static final long serialVersionUID = -4057581234220428892L;

	private final JComboBox<String> cboValues;
	private final Map<String, String> enumListValues;

	/**
	 * Constructor
	 * @param enumListValues
	 */
	public EnumField(Map<String, String> enumListValues) {
		final String[] items = enumListValues.values().toArray(new String[enumListValues.size()]);

		this.enumListValues = enumListValues;
		this.cboValues = new JComboBox<>(new DefaultComboBoxModel<>(items));
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#doAddTo(java.awt.Container, int, int,
	 * java.awt.GridBagConstraints)
	 */
	@Override
	public void doAddTo(Container grid, int row, int col, GridBagConstraints baseConstraints) {
		AutoCompleteDecorator.decorate(cboValues);
		grid.add(cboValues, baseConstraints);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#clear()
	 */
	@Override
	public void clear() {
		cboValues.setSelectedIndex(-1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#validateAndSet(boolean)
	 */
	@Override
	public void validateAndSet(boolean first) {
		for (final Map.Entry<String, String> entry : enumListValues.entrySet())
			if (entry.getValue().equals(cboValues.getSelectedItem())) {
				getSearchField().setFilterCriteria(entry.getKey());
				break;
			}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#removeFrom(java.awt.Container)
	 */
	@Override
	public void removeFrom(Container grid) {
		grid.remove(cboValues);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#syncWithSearchField(boolean)
	 */
	@Override
	public void syncWithSearchField(boolean isFirst) {
		for (final Map.Entry<String, String> entry : enumListValues.entrySet())
			if (entry.getKey().equals(getSearchField().getFilterCriteria())) {
				cboValues.setSelectedItem(entry.getValue());
				break;
			}
	}

}
