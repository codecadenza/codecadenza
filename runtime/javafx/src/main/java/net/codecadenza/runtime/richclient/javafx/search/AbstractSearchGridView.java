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
package net.codecadenza.runtime.richclient.javafx.search;

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_FETCH_NEXT_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_FETCH_PREV_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SAVE_QUERY_INPUT_DIALOG_NEW_QUERY_DEFAULT_VALUE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SAVE_QUERY_INPUT_DIALOG_NEW_QUERY_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SAVE_QUERY_MSG_OVERWRITE_QUERY;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SAVE_QUERY_MSG_QUERY_DUPLICATE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SAVE_QUERY_MSG_QUERY_SAVED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SAVE_QUERY_MSG_QUERY_SAVE_ERROR;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SAVE_QUERY_MSG_SAVE_QUERY_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SAVE_QUERY_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SEARCH_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_RESULT_NO_COUNT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_RESULT_WITH_COUNT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_SAVED_SEARCH;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_SEARCH;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_VIEW_NEXT;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_VIEW_PREV;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import javafx.scene.control.TableColumn;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.javafx.control.Action;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogButtonType;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogUtil;
import net.codecadenza.runtime.richclient.javafx.dialog.InputDialog;
import net.codecadenza.runtime.richclient.search.util.DuplicateSearchNameException;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;

/**
 * <p>
 * Abstract generic base class for grid views that provide comprehensive filter and sorting features
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the search grid view
 */
public abstract class AbstractSearchGridView<T> extends AbstractDataGridView<T> implements Countable {
	public static final int DEFAULT_MAX_FETCH_SIZE = 1000;

	protected SearchInputAction actionSearch;
	protected FetchPreviousPageAction actionPrev;
	protected FetchNextPageAction actionNext;
	protected SaveQueryAction actionSaveQuery;
	protected boolean enableNext;
	protected boolean enablePrevious;
	protected int pageIndex = 1;

	/**
	 * Constructor
	 * @param title
	 */
	protected AbstractSearchGridView(String title) {
		this(title, null, null);
	}

	/**
	 * Constructor
	 * @param title
	 * @param searchObj
	 * @param savedQueryId
	 */
	protected AbstractSearchGridView(String title, SearchDTO searchObj, Integer savedQueryId) {
		super(title, searchObj, savedQueryId);
	}

	/**
	 * Action for opening the search input dialog
	 */
	private class SearchInputAction extends Action {
		/**
		 * Constructor
		 */
		public SearchInputAction() {
			this.title = getTranslation(ACTION_SEARCH_TITLE);
			this.image = getImage(IMG_SEARCH);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()
		 */
		@Override
		public void handle() {
			refreshFormatSettings();

			final var dlg = new SearchInputDialog(null, searchObj, AbstractSearchGridView.this);
			dlg.setSize(700, 600);

			if (DialogButtonType.OK != dlg.open())
				return;

			for (final TableColumn<?, ?> col : getTableView().getColumns())
				for (final SearchFieldDTO field : searchObj.getSearchFields())
					if (col.getText().equals(field.getColLabel())) {
						col.setVisible(field.isVisible());
						break;
					}

			// Reset paging
			searchObj.setStartIndex(0);
			pageIndex = 1;

			enableNext = false;
			enablePrevious = false;

			actionNext.setEnabled(enableNext);
			actionPrev.setEnabled(enablePrevious);

			refreshView();
		}
	}

	/**
	 * Action for saving a query
	 */
	public class SaveQueryAction extends Action {
		/**
		 * Constructor
		 */
		public SaveQueryAction() {
			this.title = getTranslation(ACTION_SAVE_QUERY_TITLE);
			this.image = getImage(IMG_SAVED_SEARCH);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()
		 */
		@Override
		public void handle() {
			var queryName = "";
			final String dialogTitle = getTranslation(ACTION_SAVE_QUERY_MSG_SAVE_QUERY_TITLE);

			if (savedQueryId != null) {
				final String message = getTranslation(ACTION_SAVE_QUERY_MSG_OVERWRITE_QUERY);

				if (DialogButtonType.YES != DialogUtil.openConfirmationDialog(null, dialogTitle, message))
					return;
			}
			else {
				final String title = getTranslation(ACTION_SAVE_QUERY_INPUT_DIALOG_NEW_QUERY_TITLE);
				final String defaultValue = getTranslation(ACTION_SAVE_QUERY_INPUT_DIALOG_NEW_QUERY_DEFAULT_VALUE);
				final var dlg = new InputDialog(null, title, defaultValue);

				if (DialogButtonType.OK != dlg.open())
					return;

				queryName = dlg.getInputValue();
			}

			try {
				if (savedQueryId == null)
					SearchManager.saveSearch(AbstractSearchGridView.this.getViewID(), AbstractSearchGridView.this.searchObj, queryName);
				else
					SearchManager.overwriteSavedSearchObject(savedQueryId, searchObj);

				getStatusBar().setText(getTranslation(ACTION_SAVE_QUERY_MSG_QUERY_SAVED));
			}
			catch (final DuplicateSearchNameException _) {
				final String message = getTranslation(ACTION_SAVE_QUERY_MSG_QUERY_DUPLICATE, queryName);

				DialogUtil.openWarningDialog(null, dialogTitle, message);
			}
			catch (final Exception ex) {
				getLogger().error("Error while saving query!", ex);

				final String titleMsg = getTranslation(ACTION_SAVE_QUERY_MSG_QUERY_SAVE_ERROR);

				DialogUtil.openErrorDialog(null, title, titleMsg, ex);
			}
		}
	}

	/**
	 * Action that supports forward paging through the result set
	 */
	protected class FetchNextPageAction extends Action {
		/**
		 * Constructor
		 */
		public FetchNextPageAction() {
			this.title = getTranslation(ACTION_FETCH_NEXT_TITLE);
			this.image = getImage(IMG_VIEW_NEXT);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()
		 */
		@Override
		public void handle() {
			if (!enableNext)
				return;

			final int startIndex = searchObj.getStartIndex();
			final int maxResult = searchObj.getMaxResult();

			searchObj.setStartIndex(startIndex + maxResult);
			enablePrevious = true;

			actionPrev.setEnabled(enablePrevious);

			pageIndex++;

			refreshView();
		}
	}

	/**
	 * Action that supports backward paging through the result set
	 */
	protected class FetchPreviousPageAction extends Action {
		/**
		 * Constructor
		 */
		public FetchPreviousPageAction() {
			this.title = getTranslation(ACTION_FETCH_PREV_TITLE);
			this.image = getImage(IMG_VIEW_PREV);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()
		 */
		@Override
		public void handle() {
			if (!enablePrevious)
				return;

			final int startIndex = searchObj.getStartIndex();
			final int maxResult = searchObj.getMaxResult();

			searchObj.setStartIndex(startIndex - maxResult);

			if (pageIndex == 2)
				enablePrevious = false;

			enableNext = true;

			setEnabled(enablePrevious);

			actionNext.setEnabled(enableNext);

			pageIndex--;

			refreshView();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView#initActions()
	 */
	@Override
	protected void initActions() {
		super.initActions();

		actionSearch = new SearchInputAction();
		actionNext = new FetchNextPageAction();
		actionPrev = new FetchPreviousPageAction();
		actionSaveQuery = new SaveQueryAction();

		getToolBar().getItems().add(actionSearch.createToolbarButton());
		getToolBar().getItems().add(actionPrev.createToolbarButton());
		getToolBar().getItems().add(actionNext.createToolbarButton());
		getToolBar().getItems().add(actionSaveQuery.createToolbarButton());

		actionNext.setEnabled(false);
		actionPrev.setEnabled(false);
	}

	/**
	 * Save the search
	 */
	private void saveSearch() {
		if (savedQueryId != null)
			return;

		for (final SearchFieldDTO searchField : searchObj.getSearchFields())
			for (final TableColumn<?, ?> col : getTableView().getColumns())
				if (col.getText().equals(searchField.getColLabel())) {
					searchField.setColWidth((int) col.getWidth());
					break;
				}

		SearchManager.saveLastSearch(getViewID(), searchObj);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView#initialize()
	 */
	@Override
	public void initialize() {
		if (savedQueryId == null)
			searchObj = SearchManager.getLastSearch(getViewID());

		super.initialize();

		setOnClosed(_ -> saveSearch());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView#onPerformCountOperation()
	 */
	@Override
	protected long onPerformCountOperation() {
		if (!searchObj.isCount())
			return 0;

		getLogger().debug("Perform count operation");

		try {
			return countData();
		}
		catch (final Exception e) {
			getLogger().error("Error while performing count operation!", e);
		}

		return 0;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView#onStartSearch()
	 */
	@Override
	protected void onStartSearch() {
		super.onStartSearch();

		actionNext.setEnabled(false);
		actionPrev.setEnabled(false);
		actionSearch.setEnabled(false);

		refreshFormatSettings();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView#onSearchFailed(java.lang.Throwable)
	 */
	@Override
	protected void onSearchFailed(Throwable cause) {
		super.onSearchFailed(cause);

		actionNext.setEnabled(false);
		actionPrev.setEnabled(false);
		actionSearch.setEnabled(true);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridView#onFinishSearch(int, long, long)
	 */
	@Override
	protected void onFinishSearch(int itemsFetched, long countResult, long timeElapsed) {
		var statusMessage = "";

		if (searchObj.isCount() && countResult > 0) {
			final int pageLength = searchObj.getMaxResult();
			final long pageCount = (pageLength + countResult - 1) / pageLength;

			enableNext = pageIndex < pageCount;

			final var params = new ArrayList<>();
			params.add(itemsFetched);
			params.add(countResult);
			params.add(String.format("%.2f", timeElapsed / 1000.0));
			params.add(pageCount > 0 ? pageIndex : 0);
			params.add(pageCount);

			statusMessage = getTranslation(DATA_FETCH_ACTION_RESULT_WITH_COUNT, params.toArray());
		}
		else {
			final var params = new ArrayList<>();
			params.add(itemsFetched);
			params.add(String.format("%.2f", timeElapsed / 1000.0));

			statusMessage = getTranslation(DATA_FETCH_ACTION_RESULT_NO_COUNT, params.toArray());

			enableNext = false;
		}

		if (savedQueryId == null)
			SearchManager.saveLastSearch(getViewID(), searchObj);

		actionNext.setEnabled(enableNext);
		actionPrev.setEnabled(enablePrevious);
		actionSearch.setEnabled(true);

		getStatusBar().setText(statusMessage);
	}

	/**
	 * Refresh format settings
	 */
	public void refreshFormatSettings() {
		userFormat = FormatPreferencesManager.getFormatDTO();
		decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(ZoneId.systemDefault());
		dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(ZoneId.systemDefault());

		searchObj.setDecimalSeparator(DecimalFormatSymbols.getInstance().getDecimalSeparator());
		searchObj.setGroupingSeparator(DecimalFormatSymbols.getInstance().getGroupingSeparator());
		searchObj.setDateFormat(userFormat.getDateFormat());
		searchObj.setDateTimeFormat(userFormat.getDateTimeFormat());
		searchObj.setNumberFormat(userFormat.getDecimalFormat());
	}

}
