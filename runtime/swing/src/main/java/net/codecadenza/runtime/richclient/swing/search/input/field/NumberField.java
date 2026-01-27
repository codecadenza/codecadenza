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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.NUMBER_FIELD_MSG_INVALID_DECIMAL_NUMBER;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.NUMBER_FIELD_MSG_INVALID_NUMBER;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.NUMBER_FIELD_MSG_MISSING_INPUT;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.NUMBER_FIELD_MSG_MULT_VALUES_ON_OPERATOR;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.text.DecimalFormat;
import java.util.List;
import java.util.regex.Pattern;
import javax.swing.JTextField;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.swing.search.util.Operators;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;

/**
 * <p>
 * Input field for numbers
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class NumberField extends InputField {
	private static final long serialVersionUID = -3664903306217179790L;
	private static final Pattern TOKEN_DELIMITER_PATTERN = Pattern.compile(SearchService.TOKEN_DELIMITER_BETWEEN);

	private final JTextField txtNum;

	/**
	 * Constructor
	 */
	public NumberField() {
		this.txtNum = new JTextField(14);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#doAddTo(java.awt.Container, int, int,
	 * java.awt.GridBagConstraints)
	 */
	@Override
	public void doAddTo(Container grid, int row, int col, GridBagConstraints baseConstraints) {
		grid.add(txtNum, baseConstraints);
	}

	/**
	 * @param number
	 * @return an error message if something goes wrong
	 */
	private String tryParse(String number) {
		final var fmt = new DecimalFormat(FormatPreferencesManager.getFormatDTO().getDecimalFormat());
		String errmsg = getTranslation(NUMBER_FIELD_MSG_INVALID_NUMBER);

		try {
			switch (getSearchField().getDataType()) {
				case DOUBLE, FLOAT:
					errmsg = getTranslation(NUMBER_FIELD_MSG_INVALID_DECIMAL_NUMBER);
					fmt.parse(number);
					break;
				case LONG:
					Long.parseLong(number);
					break;
				case INTEGER:
					Integer.parseInt(number);
					break;
				case BIG_DECIMAL:
					errmsg = getTranslation(NUMBER_FIELD_MSG_INVALID_DECIMAL_NUMBER);
					fmt.setParseBigDecimal(true);
					fmt.parse(number);
					break;
				default:
					throw new AssertionError("Missing case");
			}

			return null;
		}
		catch (final Exception _) {
			return errmsg;
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#validateAndSet(boolean)
	 */
	@Override
	public void validateAndSet(boolean first) {
		final String text = txtNum.getText();

		if (text.isEmpty()) {
			getSearchInputDialog().setErrorMessage(getTranslation(NUMBER_FIELD_MSG_MISSING_INPUT));
			txtNum.requestFocus();
			return;
		}

		final List<String> vals = Operators.SPLIT_IN.split(text);

		if (!Operators.allowsMultipleValues(getCriterion().getSelectedOperator()) && vals.size() > 1) {
			getSearchInputDialog().setErrorMessage(getTranslation(NUMBER_FIELD_MSG_MULT_VALUES_ON_OPERATOR));
			return;
		}

		for (final String number : vals) {
			final String err = tryParse(number);

			if (err != null) {
				getSearchInputDialog().setErrorMessage(err);
				return;
			}
		}

		if (first)
			getSearchField().setFilterCriteria(text);
		else
			setBetweenUpperBound(getSearchField(), text);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#removeFrom(java.awt.Container)
	 */
	@Override
	public void removeFrom(Container grid) {
		grid.remove(txtNum);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#clear()
	 */
	@Override
	public void clear() {
		txtNum.setText("");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#syncWithSearchField(boolean)
	 */
	@Override
	public void syncWithSearchField(boolean isFirst) {
		final String input = getSearchField().getFilterCriteria();
		final SearchOperatorDTO op = getSearchField().getOperator();

		if (op.getValue().equals(SearchService.OPERATOR_BETWEEN)) {
			final String[] values = TOKEN_DELIMITER_PATTERN.split(input);

			if (isFirst)
				txtNum.setText(values[0]);
			else if (values.length == 2)
				txtNum.setText(values[1]);
		}
		else
			txtNum.setText(input);
	}

}
