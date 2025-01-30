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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.STRING_FIELD_MSG_MISSING_INPUT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Container;
import java.awt.GridBagConstraints;
import javax.swing.JTextField;

/**
 * <p>
 * Input field for strings
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class StringField extends InputField {
	private static final long serialVersionUID = -982974726821692382L;

	private final JTextField txtString = new JTextField();

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#doAddTo(java.awt.Container, int, int,
	 * java.awt.GridBagConstraints)
	 */
	@Override
	public void doAddTo(Container grid, int row, int col, GridBagConstraints baseConstraints) {
		grid.add(txtString, baseConstraints);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#clear()
	 */
	@Override
	public void clear() {
		txtString.setText("");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#validateAndSet(boolean)
	 */
	@Override
	public void validateAndSet(boolean first) {
		final String text = txtString.getText();

		if (text.isEmpty()) {
			getSearchInputDialog().setErrorMessage(getTranslation(STRING_FIELD_MSG_MISSING_INPUT));
			txtString.requestFocus();
			return;
		}

		getSearchField().setFilterCriteria(text);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#removeFrom(java.awt.Container)
	 */
	@Override
	public void removeFrom(Container grid) {
		grid.remove(txtString);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#syncWithSearchField(boolean)
	 */
	@Override
	public void syncWithSearchField(boolean isFirst) {
		txtString.setText(getSearchField().getFilterCriteria());
	}

}
