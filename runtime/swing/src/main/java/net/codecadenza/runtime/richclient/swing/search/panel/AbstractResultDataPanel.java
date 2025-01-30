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
package net.codecadenza.runtime.richclient.swing.search.panel;

import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel;
import net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable;
import net.codecadenza.runtime.richclient.swing.widget.JDataTable.SortKeyAccessor;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import org.jdesktop.swingx.table.TableColumnExt;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Data table that is driven by {@link SearchDTO}
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data table
 */
public abstract class AbstractResultDataPanel<T> extends AbstractDataTablePanel<T> {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = 7306359419933063112L;

	/**
	 * Default constructor
	 */
	protected AbstractResultDataPanel() {
		super(false, false);

		initColumns(null);
	}

	/**
	 * Default constructor
	 * @param searchObjectId
	 */
	protected AbstractResultDataPanel(Integer searchObjectId) {
		super(false, false);

		initColumns(searchObjectId);
	}

	/**
	 * @return a globally unique name of this view
	 */
	public abstract String getViewID();

	/**
	 * @return the initialized search object
	 */
	public abstract SearchDTO initSearch();

	/**
	 * Refresh format settings
	 */
	public void refreshFormatSettings() {
		userFormat = FormatPreferencesManager.getFormatDTO();
		decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(ZoneId.systemDefault());
		dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(ZoneId.systemDefault());

		if (getSearchObj() == null)
			return;

		getSearchObj().setDecimalSeparator(DecimalFormatSymbols.getInstance().getDecimalSeparator());
		getSearchObj().setGroupingSeparator(DecimalFormatSymbols.getInstance().getGroupingSeparator());
		getSearchObj().setDateFormat(userFormat.getDateFormat());
		getSearchObj().setDateTimeFormat(userFormat.getDateTimeFormat());
		getSearchObj().setNumberFormat(userFormat.getDecimalFormat());
	}

	/**
	 * Initialize columns
	 * @param searchObjectId
	 */
	@SuppressWarnings("unused")
	protected void initColumns(Integer searchObjectId) {
		SearchDTO searchObj = null;

		try {
			searchObj = SearchManager.getLastSearch(getViewID());
		}
		catch (final Exception e) {
			logger.error("Error while loading last saved search!", e);
		}

		if (searchObj == null)
			searchObj = initSearch();

		final var table = (JSearchDataTable<T>) this.table;
		table.initColumns(searchObj);
	}

	/**
	 * Save the last query
	 */
	public void saveLastQuery() {
		final SearchDTO searchObj = getSearchObj();

		// Save the width of all visible columns
		final List<TableColumnExt> cols = table.getAllTableColumns();

		for (final SearchFieldDTO field : searchObj.getSearchFields())
			for (final TableColumnExt col : cols) {
				if (!col.isVisible())
					continue;

				if (field.getColLabel().equals(col.getTitle())) {
					field.setColWidth(col.getWidth());
					break;
				}
			}

		SearchManager.saveLastSearch(getViewID(), searchObj);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#
	 * initTable(net.codecadenza.runtime.richclient.swing.widget.AbstractDataTableRenderer,
	 * net.codecadenza.runtime.richclient.swing.widget.JDataTable.SortKeyAccessor)
	 */
	@Override
	protected JDataTable<T> initTable(AbstractDataTableRenderer<T> renderer, SortKeyAccessor<T> sortKeyAccessor) {
		return new JSearchDataTable<>(renderer, createSortKeyAccessor());
	}

	/**
	 * @return the search object
	 */
	public SearchDTO getSearchObj() {
		return ((JSearchDataTable<?>) table).getSearchObj();
	}

	/**
	 * @param searchObj
	 */
	public void setSearchObj(SearchDTO searchObj) {
		((JSearchDataTable<?>) table).setSearchObj(searchObj);
	}

	/**
	 * Rebuild the table
	 * @param searchObj
	 */
	public void rebuildTable(SearchDTO searchObj) {
		((JSearchDataTable<?>) table).initColumns(searchObj);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#setBusy(boolean)
	 */
	@Override
	public void setBusy(boolean busy) {
		statusPanel.setBusy(busy);
	}

}
