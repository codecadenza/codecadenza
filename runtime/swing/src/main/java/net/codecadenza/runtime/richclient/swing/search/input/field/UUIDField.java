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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.NUMBER_FIELD_MSG_MULT_VALUES_ON_OPERATOR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.UUID_FIELD_MSG_INVALID_UUID;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_LIKE;
import static net.codecadenza.runtime.search.SearchService.OPERATOR_NOT_LIKE;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.util.List;
import java.util.UUID;
import javax.swing.JTextField;
import net.codecadenza.runtime.richclient.swing.search.util.Operators;
import net.codecadenza.runtime.richclient.swing.util.type.Splitter;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;

/**
 * <p>
 * Input field for UUIDs
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UUIDField extends InputField {
	private static final long serialVersionUID = 3843714592542350586L;

	private final JTextField txtUUID;

	/**
	 * Constructor
	 */
	public UUIDField() {
		this.txtUUID = new JTextField(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#doAddTo(java.awt.Container, int, int,
	 * java.awt.GridBagConstraints)
	 */
	@Override
	public void doAddTo(Container grid, int row, int col, GridBagConstraints baseConstraints) {
		grid.add(txtUUID, baseConstraints);
	}

	/**
	 * @param uuid
	 * @return an error message if something goes wrong
	 */
	private String tryParse(String uuid) {
		try {
			final SearchOperatorDTO operator = getSearchField().getOperator();

			if (operator != null && (!operator.isExpectsArgument() || operator.getValue().equals(OPERATOR_LIKE)
					|| operator.getValue().equals(OPERATOR_NOT_LIKE)))
				return null;

			if (operator == null && getSearchField().getDataType() != SearchFieldDataTypeEnum.UUID_BINARY)
				return null;

			UUID.fromString(uuid);

			return null;
		}
		catch (final IllegalArgumentException _) {
			return getTranslation(UUID_FIELD_MSG_INVALID_UUID);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#validateAndSet(boolean)
	 */
	@Override
	public void validateAndSet(boolean first) {
		final String text = txtUUID.getText();
		final List<String> vals = Splitter.on(SearchService.TOKEN_DELIMITER_IN).split(text);

		if (!Operators.allowsMultipleValues(getCriterion().getSelectedOperator()) && vals.size() > 1) {
			getSearchInputDialog().setErrorMessage(getTranslation(NUMBER_FIELD_MSG_MULT_VALUES_ON_OPERATOR));
			return;
		}

		for (final String uuid : vals) {
			final String err = tryParse(uuid);

			if (err != null) {
				getSearchInputDialog().setErrorMessage(err);
				return;
			}
		}

		if (first)
			getSearchField().setFilterCriteria(text);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#removeFrom(java.awt.Container)
	 */
	@Override
	public void removeFrom(Container grid) {
		grid.remove(txtUUID);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#clear()
	 */
	@Override
	public void clear() {
		txtUUID.setText("");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#syncWithSearchField(boolean)
	 */
	@Override
	public void syncWithSearchField(boolean isFirst) {
		txtUUID.setText(getSearchField().getFilterCriteria());
	}

}
