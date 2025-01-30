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
package net.codecadenza.runtime.richclient.javafx.tree;

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_REFRESH_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SEARCH_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SUSPEND_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_RESULT_NO_COUNT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_RESULT_WITH_COUNT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_STATUS_FETCH_DATA;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_STATUS_OP_CANCELED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_REFRESH;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_SEARCH;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_STOP;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;

import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import javafx.application.Platform;
import javafx.concurrent.Task;
import javafx.geometry.Insets;
import javafx.scene.Node;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.Tab;
import javafx.scene.control.ToolBar;
import javafx.scene.control.TreeCell;
import javafx.scene.control.TreeView;
import javafx.scene.input.DataFormat;
import javafx.scene.input.DragEvent;
import javafx.scene.input.Dragboard;
import javafx.scene.input.TransferMode;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.javafx.control.Action;
import net.codecadenza.runtime.richclient.javafx.control.StatusBar;
import net.codecadenza.runtime.richclient.javafx.control.View;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogButtonType;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogUtil;
import net.codecadenza.runtime.richclient.javafx.search.Countable;
import net.codecadenza.runtime.richclient.javafx.search.SearchInputDialog;
import net.codecadenza.runtime.richclient.search.util.SearchManager;
import net.codecadenza.runtime.search.dto.SearchDTO;
import org.slf4j.Logger;

/**
 * <p>
 * Abstract base class for tree views
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data objects for the root tree item
 */
public abstract class AbstractTreeView<T> extends Tab implements View, Countable {
	protected FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	protected DateTimeFormatter dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat())
			.withZone(ZoneId.systemDefault());
	protected DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat())
			.withZone(ZoneId.systemDefault());
	protected DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
	protected SearchDTO searchObj = new SearchDTO();
	protected ToolBar toolBar;
	protected TreeView<String> treeView;
	protected Action refreshAction;
	protected Action suspendAction;
	protected Action searchInputAction;
	protected StatusBar statusBar;
	protected TreeDataItem dragItem;
	private DataFetchTask refreshViewTask;

	/**
	 * @return a list of all items that should be added to the tree
	 * @throws Exception if the data fetch operation has failed
	 */
	protected abstract List<T> fetchRootItems() throws Exception;

	/**
	 * @param rootTreeItems
	 */
	public abstract void addRootTreeItems(List<T> rootTreeItems);

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getLogger();

	/**
	 * @param item
	 * @return the context menu for a given tree item
	 */
	@SuppressWarnings("unused")
	public ContextMenu getContextMenuForTreeItem(TreeDataItem item) {
		return null;
	}

	/**
	 * @return a search object for advanced search operations or null if this option shouldn't be added
	 */
	public SearchDTO initAdvancedSearch() {
		return null;
	}

	/**
	 * @return the quick-search bar or null if this functionality shouldn't be added
	 */
	protected Node getQuickSearchBar() {
		return null;
	}

	/**
	 * @param item
	 * @return the transfer mode or null if the item shouldn't be dragged
	 */
	@SuppressWarnings("unused")
	public TransferMode startDrag(TreeDataItem item) {
		return null;
	}

	/**
	 * Callback method that is called when a drop operation occurred
	 * @param e
	 * @param item
	 */
	@SuppressWarnings("unused")
	public void onDragDropped(DragEvent e, TreeDataItem item) {

	}

	/**
	 * @return the ID of the view
	 */
	public String getViewID() {
		return getClass().getName();
	}

	/**
	 * Callback method that is called when a count operation can be invoked
	 * @return the count result
	 * @throws Exception if the operation has failed
	 */
	protected long onPerformCountOperation() throws Exception {
		if (initAdvancedSearch() == null || !searchObj.isCount())
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

	/**
	 * Callback method that is called when a data fetch operation starts
	 */
	protected void onStartSearch() {
		getLogger().debug("Search operation started");

		// Remove all existing items from the tree view
		treeView.setRoot(new TreeDataItem(null, null));

		statusBar.showProgress();
		statusBar.setText(getTranslation(DATA_FETCH_ACTION_STATUS_FETCH_DATA));

		suspendAction.setEnabled(true);
		refreshAction.setEnabled(false);

		if (searchInputAction != null)
			searchInputAction.setEnabled(false);
	}

	/**
	 * Callback method that is called as soon as a data fetch operation is finished
	 * @param itemsFetched
	 * @param countResult
	 * @param timeElapsed
	 */
	protected void onFinishSearch(int itemsFetched, long countResult, long timeElapsed) {
		getLogger().debug("Search operation finished");

		var statusMessage = "";

		suspendAction.setEnabled(false);
		refreshAction.setEnabled(true);

		if (searchInputAction != null)
			searchInputAction.setEnabled(true);

		if (searchObj.isCount() && countResult > 0) {
			final var params = new ArrayList<>();
			params.add(itemsFetched);
			params.add(countResult);
			params.add(String.format("%.2f", timeElapsed / 1000.0));

			// The translation expects page information that cannot be provided at this point. Thus, we just add "reasonable" default
			// values!
			params.add(1);
			params.add(1);

			statusMessage = getTranslation(DATA_FETCH_ACTION_RESULT_WITH_COUNT, params.toArray());
		}
		else {
			final var params = new ArrayList<>();
			params.add(itemsFetched);
			params.add(String.format("%.2f", timeElapsed / 1000.0));

			statusMessage = getTranslation(DATA_FETCH_ACTION_RESULT_NO_COUNT, params.toArray());
		}

		statusBar.setText(statusMessage);
		statusBar.stopProgress();
	}

	/**
	 * Callback method that is called when a data fetch operation has failed
	 * @param cause the exception that caused the data fetch operation to fail. If null the operation has been cancelled by the
	 *          user!
	 */
	protected void onSearchFailed(Throwable cause) {
		getLogger().error("Search operation failed!", cause);

		statusBar.stopProgress();

		suspendAction.setEnabled(false);
		refreshAction.setEnabled(true);

		if (searchInputAction != null)
			searchInputAction.setEnabled(true);

		if (cause == null)
			statusBar.setText(getTranslation(DATA_FETCH_ACTION_STATUS_OP_CANCELED));
		else
			DialogUtil.openErrorDialog(null, getTranslation(DATA_FETCH_ACTION_MSG_QUERY_FAILED), cause);
	}

	/**
	 * Refresh the tree view
	 */
	public void refreshView() {
		new Thread(refreshViewTask = new DataFetchTask()).start();
	}

	/**
	 * Action to refresh the tree view
	 */
	private class RefreshAction extends Action {
		/**
		 * Constructor
		 */
		public RefreshAction() {
			this.title = getTranslation(ACTION_REFRESH_TITLE);
			this.image = getImage(IMG_REFRESH);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()
		 */
		@Override
		public void handle() {
			searchObj = new SearchDTO();
			searchObj.setMaxResult(1000);
			searchObj.setExactFilterMatch(true);
			searchObj.setCaseSensitive(false);
			searchObj.setCount(true);

			refreshView();
		}
	}

	/**
	 * Action to suspend the search
	 */
	protected class SuspendSearchAction extends Action {
		/**
		 * Constructor
		 */
		public SuspendSearchAction() {
			this.title = getTranslation(ACTION_SUSPEND_TITLE);
			this.image = getImage(IMG_STOP);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()
		 */
		@Override
		public void handle() {
			if (refreshViewTask != null && refreshViewTask.isRunning())
				refreshViewTask.cancel();
		}
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

			searchObj = initAdvancedSearch();

			final var dlg = new SearchInputDialog(null, searchObj, AbstractTreeView.this);
			dlg.setSize(700, 600);

			if (DialogButtonType.OK != dlg.open())
				return;

			SearchManager.saveLastSearch(getViewID(), searchObj);

			refreshView();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.control.View#initialize()
	 */
	@Override
	public void initialize() {
		getLogger().debug("Initialize tree view");

		final var panRoot = new VBox();
		toolBar = new ToolBar();
		statusBar = new StatusBar();

		treeView = new TreeView<>();
		treeView.setShowRoot(false);
		treeView.setCellFactory(param -> new DragAndDropCell());

		panRoot.getChildren().add(toolBar);

		final Node quickSearchBar = getQuickSearchBar();

		if (quickSearchBar != null)
			panRoot.getChildren().add(quickSearchBar);

		panRoot.getChildren().add(treeView);
		panRoot.getChildren().add(statusBar);
		panRoot.setPadding(new Insets(5, 5, 5, 5));

		VBox.setVgrow(treeView, Priority.ALWAYS);

		initActions();

		setContent(panRoot);

		getLogger().debug("Tree view initialization finished");
	}

	/**
	 * Initialize actions
	 */
	protected void initActions() {
		refreshAction = new RefreshAction();
		suspendAction = new SuspendSearchAction();

		toolBar.getItems().add(refreshAction.createToolbarButton());
		toolBar.getItems().add(suspendAction.createToolbarButton());

		suspendAction.setEnabled(false);

		if (initAdvancedSearch() != null) {
			searchInputAction = new SearchInputAction();
			toolBar.getItems().add(searchInputAction.createToolbarButton());
		}
	}

	/**
	 * Refresh format settings
	 */
	protected void refreshFormatSettings() {
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
	 * @see net.codecadenza.runtime.richclient.javafx.control.View#getSavedQueryId()
	 */
	@Override
	public Integer getSavedQueryId() {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.runtime.richclient.javafx.search.Countable#countData()
	 */
	@Override
	public long countData() {
		return 0;
	}

	/**
	 * Implementation of a tree cell that supports drag and drop operations
	 */
	private class DragAndDropCell extends TreeCell<String> {
		/**
		 * Constructor
		 */
		public DragAndDropCell() {
			setOnDragDetected(e -> {
				if (!(getTreeItem() instanceof final TreeDataItem treeItem))
					return;

				final TransferMode mode = startDrag(treeItem);

				if (mode == null)
					return;

				dragItem = treeItem;

				final var content = new HashMap<DataFormat, Object>();
				content.put(DataFormat.PLAIN_TEXT, dragItem.getValue() == null ? "" : dragItem.getValue());

				final Dragboard dragBoard = startDragAndDrop(mode);
				dragBoard.setContent(content);

				e.consume();
			});

			setOnDragDone(DragEvent::consume);

			setOnDragOver(e -> {
				e.acceptTransferModes(TransferMode.COPY, TransferMode.MOVE);
				e.consume();
			});

			setOnDragDropped(e -> {
				// We don't have to inform a listener about a drop operation if either the drag or the drop item is missing!
				if (getTreeItem() == null || dragItem == null)
					return;

				if (!(getTreeItem() instanceof final TreeDataItem treeItem))
					return;

				onDragDropped(e, treeItem);
				dragItem = null;
			});
		}

		/*
		 * (non-Javadoc)
		 * @see javafx.scene.control.Cell#updateItem(java.lang.Object, boolean)
		 */
		@Override
		protected void updateItem(String item, boolean empty) {
			super.updateItem(item, empty);

			if (empty) {
				setText(null);
				setGraphic(null);
				setContextMenu(null);
				return;
			}

			if (getTreeItem() != null) {
				setText(item);
				setGraphic(getTreeItem().getGraphic());
				setContextMenu(getContextMenuForTreeItem((TreeDataItem) getTreeItem()));
			}
		}
	}

	/**
	 * Task for fetching data in a separate thread
	 */
	private class DataFetchTask extends Task<Void> {
		/*
		 * (non-Javadoc)
		 * @see javafx.concurrent.Task#cancelled()
		 */
		@Override
		protected void cancelled() {
			onSearchFailed(null);
		}

		/*
		 * (non-Javadoc)
		 * @see javafx.concurrent.Task#call()
		 */
		@Override
		public Void call() throws Exception {
			final List<T> data;
			final long start = System.currentTimeMillis();
			final long countResult;

			Platform.runLater(AbstractTreeView.this::onStartSearch);

			data = fetchRootItems();

			if (isDone())
				return null;

			countResult = onPerformCountOperation();

			if (isDone())
				return null;

			Platform.runLater(() -> {
				if (data != null) {
					addRootTreeItems(data);

					onFinishSearch(data.size(), countResult, System.currentTimeMillis() - start);
				}
				else
					onFinishSearch(0, countResult, System.currentTimeMillis() - start);
			});

			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see javafx.concurrent.Task#failed()
		 */
		@Override
		protected void failed() {
			onSearchFailed(getException());
		}
	}

}
