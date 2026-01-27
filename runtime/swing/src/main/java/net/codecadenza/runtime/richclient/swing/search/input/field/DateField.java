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

import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.DATE_FIELD_MSG_INVALID_DATE;
import static net.codecadenza.runtime.richclient.swing.i18n.I18NSwing.getTranslation;

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.text.DateFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.regex.Pattern;
import javax.swing.JTextField;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchOperatorDTO;

/**
 * <p>
 * Input field for date type
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DateField extends InputField {
	private static final Pattern TOKEN_DELIMITER_PATTERN = Pattern.compile(SearchService.TOKEN_DELIMITER_BETWEEN);
	private static final long serialVersionUID = -4097044211512860233L;

	private final DateFormat format;
	private final JTextField txtInput;

	/**
	 * Constructor
	 * @param format
	 */
	public DateField(DateFormat format) {
		this.format = format;
		this.txtInput = new JTextField();
	}

	/**
	 * Constructor
	 * @param isDateTime
	 */
	public DateField(boolean isDateTime) {
		this(getFormat(isDateTime));
	}

	/**
	 * @param isDateTime
	 * @return the selected date format
	 */
	private static DateFormat getFormat(boolean isDateTime) {
		String fmt;

		if (isDateTime)
			fmt = FormatPreferencesManager.getFormatDTO().getDateTimeFormat();
		else
			fmt = FormatPreferencesManager.getFormatDTO().getDateFormat();

		return new SimpleDateFormat(fmt);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#doAddTo(java.awt.Container, int, int,
	 * java.awt.GridBagConstraints)
	 */
	@Override
	public void doAddTo(Container grid, int row, int col, GridBagConstraints baseConstraints) {
		grid.add(txtInput, baseConstraints);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#clear()
	 */
	@Override
	public void clear() {
		txtInput.setText("");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#validateAndSet(boolean)
	 */
	@Override
	public void validateAndSet(boolean first) {
		final String dateText = txtInput.getText();

		if (dateText.isEmpty()) {
			getSearchInputDialog().setErrorMessage(getTranslation(DATE_FIELD_MSG_INVALID_DATE));
			txtInput.requestFocus();
		}
		else {
			try {
				format.parse(dateText);
			}
			catch (final ParseException _) {
				getSearchInputDialog().setErrorMessage(getTranslation(DATE_FIELD_MSG_INVALID_DATE));
				return;
			}

			if (first)
				getSearchField().setFilterCriteria(dateText);
			else
				setBetweenUpperBound(getSearchField(), dateText);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.input.field.InputField#removeFrom(java.awt.Container)
	 */
	@Override
	public void removeFrom(Container grid) {
		grid.remove(txtInput);
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
				txtInput.setText(values[0]);
			else if (values.length == 2)
				txtInput.setText(values[1]);
		}
		else
			txtInput.setText(getSearchField().getFilterCriteria());
	}

}
