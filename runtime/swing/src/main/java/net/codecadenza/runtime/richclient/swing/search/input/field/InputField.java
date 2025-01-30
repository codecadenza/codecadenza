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
import java.io.Serializable;
import net.codecadenza.runtime.richclient.swing.search.input.SearchInputDialog;
import net.codecadenza.runtime.richclient.swing.search.input.components.Criterion;
import net.codecadenza.runtime.search.SearchService;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;

/**
 * <p>
 * A InputField lets the user edit a {@link SearchFieldDTO}. It is used by {@link Criterion}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class InputField implements Serializable {
	private static final long serialVersionUID = 8690711864069166778L;

	/**
	 * Draw the input field on the grid at the given position. It may span over two columns in the grid!
	 * @param grid
	 * @param row
	 * @param col
	 * @param baseConstraints
	 */
	protected abstract void doAddTo(Container grid, int row, int col, GridBagConstraints baseConstraints);

	/**
	 * Remove the input field from the grid. The grid should be the same instance where this input field has been added before!
	 * @param grid
	 */
	public abstract void removeFrom(Container grid);

	/**
	 * Validate the input and set the {@link SearchFieldDTO} or set the error message of the dialog. In every row there are one or
	 * two (operator between) input fields
	 * @param first true if it is the leftmost input field in its row
	 */
	public abstract void validateAndSet(boolean first);

	/**
	 * Reset the input field
	 */
	public abstract void clear();

	/**
	 * Model that is used to store the user's input
	 */
	private SearchFieldDTO field;

	/**
	 * Dialog where this input field is contained in
	 */
	private SearchInputDialog dlg;

	/**
	 * The criterion where this input field is contained in
	 */
	private Criterion criterion;

	/**
	 * Draw the input field on the grid
	 * @param grid
	 * @param row
	 * @param col
	 * @param baseConstraints
	 */
	public void addTo(Container grid, int row, int col, GridBagConstraints baseConstraints) {
		if (dlg == null || criterion == null || field == null)
			throw new IllegalStateException();

		doAddTo(grid, row, col, baseConstraints);
	}

	/**
	 * @param field
	 */
	protected void setSearchField(SearchFieldDTO field) {
		this.field = field;
	}

	/**
	 * @return the corresponding {@link SearchFieldDTO}
	 */
	public SearchFieldDTO getSearchField() {
		return field;
	}

	/**
	 * Factory method to create an input field
	 * @param field
	 * @param c
	 * @return the generated input field
	 */
	public static InputField create(SearchFieldDTO field, Criterion c) {
		final SearchFieldDataTypeEnum type = field.getDataType();
		InputField input;

		switch (type) {
			case BOOLEAN:
				input = new BoolField();
				break;
			case STRING:
				if (field.getListOfValues() != null)
					input = new LOVField();
				else
					input = new StringField();

				break;
			case UUID_STRING:
				input = new UUIDField();
				break;
			case UUID_BINARY:
				input = new UUIDField();
				break;
			case ENUM:
				input = new EnumField(field.getEnumListValues());
				break;
			case LOCAL_DATE, LOCAL_DATE_TIME, DATE, GREGORIAN_CALENDAR:
				input = new DateField(field.isDateTimeFormat());
				break;
			case BIG_DECIMAL, DOUBLE, FLOAT, INTEGER, LONG:
				input = new NumberField();
				break;
			default:
				throw new AssertionError("missing case");
		}

		input.setSearchField(field);
		input.setCriterion(c);

		return input;
	}

	/**
	 * @param dlg
	 */
	public void setSearchInputDialog(SearchInputDialog dlg) {
		this.dlg = dlg;
	}

	/**
	 * @return the corresponding dialog the input field is placed on
	 */
	public SearchInputDialog getSearchInputDialog() {
		return dlg;
	}

	/**
	 * @param criterion
	 */
	public void setCriterion(Criterion criterion) {
		this.criterion = criterion;
	}

	/**
	 * @return the criterion
	 */
	public Criterion getCriterion() {
		return criterion;
	}

	/**
	 * @param field
	 * @param upperBound
	 */
	protected static void setBetweenUpperBound(SearchFieldDTO field, String upperBound) {
		final String lowerBound = field.getFilterCriteria();
		field.setFilterCriteria(lowerBound + SearchService.TOKEN_DELIMITER_BETWEEN + upperBound);
	}

	/**
	 * Only gets called if an operator is selected
	 * @param isFirst
	 */
	public abstract void syncWithSearchField(boolean isFirst);

}
