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
import javax.swing.JMenuItem;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.richclient.swing.search.Countable;
import net.codecadenza.runtime.richclient.swing.search.actions.DataFetchAction;
import net.codecadenza.runtime.richclient.swing.search.actions.FetchNextPageAction;
import net.codecadenza.runtime.richclient.swing.search.actions.FetchPreviousPageAction;
import net.codecadenza.runtime.richclient.swing.search.actions.SaveQueryAction;
import net.codecadenza.runtime.richclient.swing.search.actions.SearchInputAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.CopyCellToClipboardAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.CopyColumnToClipboardAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.CopyRowToClipboardAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.ExportXLSAction;
import net.codecadenza.runtime.richclient.swing.widget.actions.RefreshAction;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * <p>
 * Panel to display data in a table including a generic search functionality
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the search result panel
 */
public abstract class AbstractSearchResultDataPanel<T> extends AbstractResultDataPanel<T> implements Countable {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final long serialVersionUID = 748414151793202867L;

	private int pageIndex = 1;
	private SearchInputAction searchInputAction;
	private FetchNextPageAction pageNextAction;
	private FetchPreviousPageAction pagePrevAction;
	private DataFetchAction<T> fetchAction;
	private SaveQueryAction saveQueryAction;
	private boolean userDefQuery;
	protected int searchObjectId;

	/**
	 * Constructor
	 * @param savedSearchId
	 */
	protected AbstractSearchResultDataPanel(Integer savedSearchId) {
		super(savedSearchId);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.search.AbstractSearchDataPanel#initColumns(java.lang.Integer)
	 */
	@Override
	protected void initColumns(Integer savedSearchId) {
		final String primaryId = getViewID();
		SearchDTO searchObj = null;

		if (savedSearchId != null) {
			userDefQuery = true;
			searchObjectId = savedSearchId;
			searchObj = SearchManager.getSavedSearch(searchObjectId);
		}
		else {
			userDefQuery = false;

			try {
				searchObj = SearchManager.getLastSearch(primaryId);
			}
			catch (final Exception e) {
				logger.error("Error while loading last saved search!", e);
			}

			if (searchObj == null)
				searchObj = initSearch();
		}

		final var table = (JSearchDataTable<T>) this.table;
		table.initColumns(searchObj);

		// Disable all toolbar actions!
		pageNextAction.setEnabled(false);
		pagePrevAction.setEnabled(false);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#initActions()
	 */
	@Override
	protected void initActions() {
		// We may not call super() at this point!
		fetchAction = new DataFetchAction<>(this);
		cellCopyAction = new CopyCellToClipboardAction(table);
		rowCopyAction = new CopyRowToClipboardAction(table);
		columnCopyAction = new CopyColumnToClipboardAction(table);
		xlsExportAction = new ExportXLSAction(this);
		searchInputAction = new SearchInputAction(this);
		pageNextAction = new FetchNextPageAction(this);
		pagePrevAction = new FetchPreviousPageAction(this);
		saveQueryAction = new SaveQueryAction(this);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#initToolBar()
	 */
	@Override
	protected void initToolBar() {
		// We may not call super() at this point!
		toolBar.add(fetchAction);
		toolBar.add(searchInputAction);
		toolBar.add(pagePrevAction);
		toolBar.add(pageNextAction);
		toolBar.add(xlsExportAction);
		toolBar.add(saveQueryAction);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.swing.widget.AbstractDataTablePanel#initPopUpMenu()
	 */
	@Override
	protected void initPopUpMenu() {
		// We may not call super() at this point!
		final var menuItemRefresh = new JMenuItem();
		menuItemRefresh.setAction(fetchAction);

		popupMenu.add(menuItemRefresh);

		final var menuItemExport = new JMenuItem();
		menuItemExport.setAction(xlsExportAction);

		popupMenu.add(menuItemExport);

		final var menuItemCopyCell = new JMenuItem();
		menuItemCopyCell.setAction(cellCopyAction);

		popupMenu.add(menuItemCopyCell);

		final var menuItemCopyRow = new JMenuItem();
		menuItemCopyRow.setAction(rowCopyAction);

		popupMenu.add(menuItemCopyRow);

		final var menuItemCopyColumn = new JMenuItem();
		menuItemCopyColumn.setAction(columnCopyAction);

		popupMenu.add(menuItemCopyColumn);
	}

	/**
	 * @return the page index
	 */
	public int getPageIndex() {
		return pageIndex;
	}

	/**
	 * @param index
	 */
	public void setPageIndex(int index) {
		this.pageIndex = index;
	}

	/**
	 * @return the object that implements the {@link Countable} interface
	 */
	public Countable getCountable() {
		return this;
	}

	/**
	 * @return the refresh action
	 */
	public RefreshAction getRefreshAction() {
		return refreshAction;
	}

	/**
	 * @return true if the query represents a saved query
	 */
	public boolean isUserDefQuery() {
		return userDefQuery;
	}

	/**
	 * @return the ID of the search object
	 */
	public int getSearchObjectId() {
		return searchObjectId;
	}

	/**
	 * @return the action to fetch the next page
	 */
	public FetchNextPageAction getNextPageAction() {
		return pageNextAction;
	}

	/**
	 * @return the action to fetch the previous page
	 */
	public FetchPreviousPageAction getPrevPageAction() {
		return pagePrevAction;
	}

	/**
	 * @return the action that is responsible to fetch the data
	 */
	public DataFetchAction<T> getDataFetchAction() {
		return fetchAction;
	}

	/**
	 * Perform the data fetch operation
	 */
	public void performDataFetch() {
		fetchAction.actionPerformed(null);
	}

}
