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
package net.codecadenza.runtime.richclient.eclipse.search;

import net.codecadenza.runtime.richclient.eclipse.action.AbstractExportXLSXAction;
import net.codecadenza.runtime.richclient.eclipse.action.ExportXLSXAction;
import net.codecadenza.runtime.richclient.eclipse.rap.services.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.search.util.DuplicateSearchNameException;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Shell;

/**
 * <p>
 * Abstract view that displays search results in a table viewer
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the search result view
 */
public abstract class AbstractSearchResultView<T> extends __AbstractSearchResultView<T> {
	protected ExportXLSXAction exportAction;

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractResultView#getExportAction()
	 */
	@Override
	protected AbstractExportXLSXAction getExportAction() {
		return exportAction;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#getSearchInputDialog()
	 */
	@Override
	public __SearchInputDialog getSearchInputDialog() {
		return new SearchInputDialog(parentShell, searchObj, this);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#__createActions()
	 */
	@Override
	protected void __createActions() {
		super.__createActions();

		exportAction = new ExportXLSXAction(parentShell, this);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#__initializeToolBar()
	 */
	@Override
	protected void __initializeToolBar() {
		super.__initializeToolBar();

		toolBarManager.add(exportAction);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#init(org.eclipse.swt.widgets.
	 * Composite, java.lang.Integer, org.eclipse.swt.widgets.Shell)
	 */
	@Override
	public void init(Composite parent, Integer savedSearchId, Shell parentShell) {
		super.init(parent, savedSearchId, parentShell);

		__createActions();
		__initializeToolBar();

		if (isUserDefQuery || lastSearchFound) {
			// Reset paging
			searchObj.setStartIndex(0);

			pageIndex = 1;
		}

		executeQuery(true);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#getFormatPreferences()
	 */
	@Override
	public FormatDTO getFormatPreferences() {
		return FormatPreferencesManager.getFormatDTO();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#saveSearch(java.lang.String,
	 * net.codecadenza.runtime.search.dto.SearchDTO, java.lang.String)
	 */
	@Override
	public void saveSearch(String viewName, SearchDTO searchDTO, String name) throws DuplicateSearchNameException {
		SearchManager.saveSearch(viewName, searchDTO, name);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#saveLastSearch(java.lang.String,
	 * net.codecadenza.runtime.search.dto.SearchDTO)
	 */
	@Override
	public void saveLastSearch(String viewName, SearchDTO searchDTO) {
		SearchManager.saveLastSearch(viewName, searchDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#overwriteSavedSearchObject(int,
	 * net.codecadenza.runtime.search.dto.SearchDTO)
	 */
	@Override
	public void overwriteSavedSearchObject(int id, SearchDTO searchDTO) {
		SearchManager.overwriteSavedSearchObject(id, searchDTO);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#getSavedSearch(int)
	 */
	@Override
	public SearchDTO getSavedSearch(int id) {
		return SearchManager.getSavedSearch(id);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.eclipse.search.__AbstractSearchResultView#getLastSearch(java.lang.String)
	 */
	@Override
	public SearchDTO getLastSearch(String viewName) {
		return SearchManager.getLastSearch(viewName);
	}

	/**
	 * Dispose internal resources
	 */
	public void dispose() {

	}

}
