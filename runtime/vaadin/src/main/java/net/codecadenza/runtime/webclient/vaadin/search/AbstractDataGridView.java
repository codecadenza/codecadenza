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
package net.codecadenza.runtime.webclient.vaadin.search;

import com.vaadin.flow.component.AttachEvent;
import com.vaadin.flow.component.Unit;
import com.vaadin.flow.component.grid.Grid.Column;
import com.vaadin.flow.function.ValueProvider;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.Date;
import java.util.GregorianCalendar;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid;
import net.codecadenza.runtime.webclient.vaadin.i18n.I18NService;
import net.codecadenza.runtime.webclient.vaadin.util.PreferencesStore;

/**
 * <p>
 * Component that provides a tabular view for given domain objects and an action for creating an XLSX export file including all
 * visible rows and columns
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid
 */
public abstract class AbstractDataGridView<T> extends AbstractDataGrid<T> {
	private static final long serialVersionUID = 5634404009064753810L;

	protected SearchDTO searchObj;

	/**
	 * Initialize the search object
	 * @return the initialized search object for this view
	 */
	public abstract SearchDTO init();

	/**
	 * Constructor
	 * @param i18n
	 * @param preferences
	 */
	protected AbstractDataGridView(I18NService i18n, PreferencesStore preferences) {
		super(i18n, preferences);

		setId("dataTable");
		setAddButtonBar(true);
	}

	/**
	 * @return the number of items that this query would return
	 */
	public Long performCountOperation() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.webclient.vaadin.component.AbstractDataGrid#initGridColumns()
	 */
	@Override
	@SuppressWarnings("unchecked")
	public void initGridColumns() {
		if (searchObj == null)
			searchObj = init();

		searchObj.setDateFormat(preferences.getDateFormat());
		searchObj.setDateTimeFormat(preferences.getDateTimeFormat());
		searchObj.setNumberFormat(preferences.getNumberFormat());

		// Initialize the grid by using the search fields
		for (final SearchFieldDTO field : searchObj.getSearchFields()) {
			if (!field.isVisible())
				continue;

			final var beanField = (BindingSearchField<T>) field;
			final var valueProvider = beanField.getBinding();
			final Column<T> column;

			if (field.getDataType() == SearchFieldDataTypeEnum.DATE) {
				if (field.isDateTimeFormat())
					column = addDateTimeColumn(beanField.getColLabel(), (ValueProvider<T, Date>) valueProvider);
				else
					column = addDateColumn(beanField.getColLabel(), (ValueProvider<T, Date>) valueProvider);
			}
			else if (field.getDataType() == SearchFieldDataTypeEnum.LOCAL_DATE)
				column = addLocalDateColumn(beanField.getColLabel(), (ValueProvider<T, LocalDate>) valueProvider);
			else if (field.getDataType() == SearchFieldDataTypeEnum.LOCAL_DATE_TIME)
				column = addLocalDateTimeColumn(beanField.getColLabel(), (ValueProvider<T, LocalDateTime>) valueProvider);
			else if (field.getDataType() == SearchFieldDataTypeEnum.GREGORIAN_CALENDAR)
				column = addGregorianCalendarColumn(beanField.getColLabel(), (ValueProvider<T, GregorianCalendar>) valueProvider);
			else if (field.getDataType() == SearchFieldDataTypeEnum.ENUM)
				column = addEnumColumn(beanField.getColLabel(), (ValueProvider<T, Enum<?>>) valueProvider, field.getEnumListValues());
			else if (field.getDataType() == SearchFieldDataTypeEnum.DOUBLE)
				column = addDoubleColumn(beanField.getColLabel(), (ValueProvider<T, Double>) valueProvider);
			else if (field.getDataType() == SearchFieldDataTypeEnum.FLOAT)
				column = addFloatColumn(beanField.getColLabel(), (ValueProvider<T, Float>) valueProvider);
			else
				column = addColumn(beanField.getColLabel(), valueProvider);

			column.setWidth(Integer.toString(field.getColWidth()) + Unit.PIXELS);
			column.setHeader(beanField.getColLabel());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see com.vaadin.flow.component.Component#onAttach(com.vaadin.flow.component.AttachEvent)
	 */
	@Override
	public void onAttach(AttachEvent event) {
		initGridColumns();

		// Fetch the data when opening the view
		refresh();
	}

}
