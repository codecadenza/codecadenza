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

import java.text.DecimalFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.List;
import javafx.beans.property.SimpleStringProperty;
import javafx.collections.FXCollections;
import javafx.collections.ListChangeListener;
import javafx.collections.ObservableList;
import javafx.scene.control.Tab;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TableView;
import javafx.scene.control.ToolBar;
import javafx.scene.input.KeyCodeCombination;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.javafx.control.Action;
import net.codecadenza.runtime.richclient.javafx.control.StatusBar;
import net.codecadenza.runtime.richclient.javafx.control.View;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import org.slf4j.Logger;

/**
 * <p>
 * Abstract generic base class for grid views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid view
 */
public abstract class AbstractDataGridView<T> extends Tab implements View {
	private static final long DEFAULT_COUNT_RESULT = 0;

	protected SearchDTO searchObj = new SearchDTO();
	protected FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	protected DateTimeFormatter dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat())
			.withZone(ZoneId.systemDefault());
	protected DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat())
			.withZone(ZoneId.systemDefault());
	protected DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
	private AbstractDataGridPanel<T> gridPanel;
	protected String title;
	protected Integer savedQueryId;

	/**
	 * Constructor
	 * @param title
	 */
	protected AbstractDataGridView(String title) {
		this(title, null, null);
	}

	/**
	 * Constructor
	 * @param title
	 * @param searchObj
	 * @param savedQueryId
	 */
	protected AbstractDataGridView(String title, SearchDTO searchObj, Integer savedQueryId) {
		this.title = title;
		this.searchObj = searchObj;
		this.savedQueryId = savedQueryId;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.control.View#getSavedQueryId()
	 */
	@Override
	public Integer getSavedQueryId() {
		return savedQueryId;
	}

	/**
	 * @return the ID of the view
	 */
	public String getViewID() {
		return getClass().getName();
	}

	/**
	 * @param element
	 * @param colIndex
	 * @return the text of a given cell
	 */
	protected abstract String getCellText(T element, int colIndex);

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getLogger();

	/**
	 * Initialize the columns
	 * @return a list containing all columns
	 */
	protected ObservableList<TableColumn<T, String>> initColumns() {
		final ObservableList<TableColumn<T, String>> columnList = FXCollections.observableArrayList();
		final List<SearchFieldDTO> fieldList = searchObj.getSearchFields().stream()
				.sorted((a, b) -> Integer.compare(a.getColOrder(), b.getColOrder())).toList();

		fieldList.forEach(searchField -> {
			final var col = new TableColumn<T, String>(searchField.getColLabel());
			col.setPrefWidth(searchField.getColWidth());
			col.setUserData(searchField.getDataType());
			col.setVisible(searchField.isVisible());
			col.setCellValueFactory(
					param -> new SimpleStringProperty(getCellText(param.getValue(), searchField.getOriginalColumnIndex())));

			columnList.add(col);
		});

		return columnList;
	}

	/**
	 * Initialize the search input object
	 * @return the initialized search object
	 */
	protected abstract SearchDTO initSearchObject();

	/**
	 * @return the items to be displayed in grid panel
	 * @throws Exception if the data fetch operation has failed
	 */
	protected abstract List<T> fetchData() throws Exception;

	/**
	 * Callback method for double-click events
	 * @param item
	 */
	@SuppressWarnings("unused")
	protected void onDoubleClick(T item) {

	}

	/**
	 * Callback method that is called as soon as the 'ENTER' key is pressed
	 * @param item
	 */
	@SuppressWarnings("unused")
	protected void onEnterPressed(T item) {

	}

	/**
	 * Callback method that is called as soon as the 'DELETE' key is pressed
	 * @param item
	 */
	@SuppressWarnings("unused")
	protected void onDeletePressed(T item) {

	}

	/**
	 * Callback method that is called as soon as a data fetch operation starts
	 */
	protected void onStartSearch() {

	}

	/**
	 * Callback method that is called as soon as a data fetch operation is finished
	 * @param itemsFetched
	 * @param countResult
	 * @param timeElapsed
	 */
	@SuppressWarnings("unused")
	protected void onFinishSearch(int itemsFetched, long countResult, long timeElapsed) {

	}

	/**
	 * Callback method that is called as soon as a data fetch operation has failed
	 * @param cause the exception that caused the data fetch operation to fail. If null the operation has been cancelled by the
	 *          user!
	 */
	@SuppressWarnings("unused")
	protected void onSearchFailed(Throwable cause) {

	}

	/**
	 * Callback method that is called as soon as a count operation can be invoked
	 * @return the count result
	 * @throws Exception if the count operation has failed
	 */
	protected long onPerformCountOperation() throws Exception {
		return DEFAULT_COUNT_RESULT;
	}

	/**
	 * @return the selected item
	 */
	public T getSelectedItem() {
		return gridPanel.getSelectedItem();
	}

	/**
	 * @return the toolbar
	 */
	protected ToolBar getToolBar() {
		return gridPanel.getToolBar();
	}

	/**
	 * @return the table view
	 */
	protected TableView<T> getTableView() {
		return gridPanel.getTableView();
	}

	/**
	 * @return the status bar
	 */
	protected StatusBar getStatusBar() {
		return gridPanel.getStatusBar();
	}

	/**
	 * Add an action to the context menu
	 * @param action
	 */
	protected void addActionToContextMenu(Action action) {
		gridPanel.addActionToContextMenu(action);
	}

	/**
	 * Add an action to the context menu
	 * @param action
	 * @param keyCodeCombination
	 */
	protected void addActionToContextMenu(Action action, KeyCodeCombination keyCodeCombination) {
		gridPanel.addActionToContextMenu(action, keyCodeCombination);
	}

	/**
	 * Add an action to the toolbar
	 * @param action
	 */
	protected void addActionToToolBar(Action action) {
		gridPanel.addActionToToolBar(action);
	}

	/**
	 * Refresh the view
	 */
	protected void refreshView() {
		gridPanel.refreshView();
	}

	/**
	 * Initialize actions
	 */
	protected void initActions() {
		gridPanel.initActions();
	}

	/**
	 * Synchronize the order of the visual columns and the search fields
	 */
	private void changeColumnOrder() {
		int colIndex = 0;

		for (final TableColumn<?, ?> col : getTableView().getColumns())
			for (final SearchFieldDTO searchField : searchObj.getSearchFields())
				if (col.getText().equals(searchField.getColLabel())) {
					searchField.setColOrder(colIndex++);
					break;
				}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.control.View#initialize()
	 */
	@Override
	public void initialize() {
		getLogger().debug("Initialize view");

		gridPanel = new AbstractDataGridPanel<>(null) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#getCellText(java.lang.Object, int)
			 */
			@Override
			protected String getCellText(T element, int colIndex) {
				return AbstractDataGridView.this.getCellText(element, colIndex);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#initColumns()
			 */
			@Override
			protected ObservableList<TableColumn<T, String>> initColumns() {
				return AbstractDataGridView.this.initColumns();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#fetchData()
			 */
			@Override
			protected List<T> fetchData() throws Exception {
				return AbstractDataGridView.this.fetchData();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#onStartSearch()
			 */
			@Override
			protected void onStartSearch() {
				super.onStartSearch();

				AbstractDataGridView.this.onStartSearch();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#onFinishSearch(int, long, long)
			 */
			@Override
			protected void onFinishSearch(int itemsFetched, long countResult, long timeElapsed) {
				super.onFinishSearch(itemsFetched, countResult, timeElapsed);

				AbstractDataGridView.this.onFinishSearch(itemsFetched, countResult, timeElapsed);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#onPerformCountOperation()
			 */
			@Override
			protected long onPerformCountOperation() throws Exception {
				return AbstractDataGridView.this.onPerformCountOperation();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#onSearchFailed(java.lang.Throwable)
			 */
			@Override
			protected void onSearchFailed(Throwable cause) {
				super.onSearchFailed(cause);

				AbstractDataGridView.this.onSearchFailed(cause);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#onDoubleClick(java.lang.Object)
			 */
			@Override
			protected void onDoubleClick(T item) {
				AbstractDataGridView.this.onDoubleClick(item);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#onDeletePressed(java.lang.Object)
			 */
			@Override
			protected void onDeletePressed(T item) {
				AbstractDataGridView.this.onDeletePressed(item);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#onEnterPressed(java.lang.Object)
			 */
			@Override
			protected void onEnterPressed(T item) {
				AbstractDataGridView.this.onEnterPressed(item);
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.javafx.search.AbstractDataGridPanel#getLogger()
			 */
			@Override
			protected Logger getLogger() {
				return AbstractDataGridView.this.getLogger();
			}
		};

		setContent(gridPanel);

		if (searchObj == null)
			searchObj = initSearchObject();

		gridPanel.initialize();

		// Add the columns to the table view
		gridPanel.getTableView().getColumns().addAll(initColumns());

		initActions();

		refreshView();

		getTableView().getColumns().addListener((ListChangeListener<TableColumn<?, ?>>) _ -> changeColumnOrder());

		getLogger().debug("View initialization finished");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.control.View#getTab()
	 */
	@Override
	public Tab getTab() {
		return this;
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		return title;
	}

}
