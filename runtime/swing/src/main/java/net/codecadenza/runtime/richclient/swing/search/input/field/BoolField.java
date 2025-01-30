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
import javax.swing.DefaultComboBoxModel;
import javax.swing.JComboBox;

/**
 * <p>
 * Input field for boolean types
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class BoolField extends InputField {
	private static final long serialVersionUID = -115878791373942028L;

	private final JComboBox<String> cboBool;

	/**
	 * Constructor
	 */
	public BoolField() {
		this.cboBool = new JComboBox<>();
		this.cboBool.setEditable(false);

		// At this point a translation cannot be added as 'true' and 'false' are keywords for search operations!
		final var model = (DefaultComboBoxModel<String>) cboBool.getModel();
		model.addElement("");
		model.addElement("true");
		model.addElement("false");

		clear();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#doAddTo(java.awt.Container, int, int,
	 * java.awt.GridBagConstraints)
	 */
	@Override
	public void doAddTo(Container grid, int row, int col, GridBagConstraints baseConstraints) {
		grid.add(cboBool, baseConstraints);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#clear()
	 */
	@Override
	public void clear() {
		cboBool.setSelectedIndex(0);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#validateAndSet(boolean)
	 */
	@Override
	public void validateAndSet(boolean first) {
		String selItem = (String) cboBool.getSelectedItem();

		if (selItem != null && selItem.isEmpty())
			selItem = null;

		getSearchField().setFilterCriteria(selItem);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#removeFrom(java.awt.Container)
	 */
	@Override
	public void removeFrom(Container grid) {
		grid.remove(cboBool);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#syncWithSearchField(boolean)
	 */
	@Override
	public void syncWithSearchField(boolean isFirst) {
		cboBool.setSelectedItem(getSearchField().getFilterCriteria());
	}

}
