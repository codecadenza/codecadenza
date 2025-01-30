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

import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_COPY_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_EXPORT_DIALOG_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_EXPORT_MSG_EXPORT_ERROR;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_EXPORT_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_EXPORT_XLSX_FILTER_DESC;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_REFRESH_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.ACTION_SUSPEND_TITLE;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_RESULT_NO_COUNT;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_STATUS_FETCH_DATA;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.DATA_FETCH_ACTION_STATUS_OP_CANCELED;
import static net.codecadenza.runtime.richclient.javafx.i18n.I18NJavaFX.getTranslation;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_COPY;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_EXPORT_EXCEL;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_REFRESH;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.IMG_STOP;
import static net.codecadenza.runtime.richclient.javafx.image.ImageLoader.getImage;

import java.awt.Desktop;
import java.io.File;
import java.text.DecimalFormat;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import javafx.application.Platform;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.concurrent.Task;
import javafx.geometry.Insets;
import javafx.scene.control.Button;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import javafx.scene.control.SelectionMode;
import javafx.scene.control.TableColumn;
import javafx.scene.control.TablePosition;
import javafx.scene.control.TableView;
import javafx.scene.control.ToolBar;
import javafx.scene.input.Clipboard;
import javafx.scene.input.ClipboardContent;
import javafx.scene.input.KeyCode;
import javafx.scene.input.KeyCodeCombination;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;
import javafx.stage.Window;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.format.FormatPreferencesManager;
import net.codecadenza.runtime.richclient.javafx.control.Action;
import net.codecadenza.runtime.richclient.javafx.control.StatusBar;
import net.codecadenza.runtime.richclient.javafx.dialog.DialogUtil;
import net.codecadenza.runtime.richclient.javafx.util.XLSXExportUtility;
import org.slf4j.Logger;

/**
 * <p>
 * Abstract generic base class for grid panels
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the data grid
 */
public abstract class AbstractDataGridPanel<T> extends VBox {
	private static final String DEFAULT_EXPORT_FILE_NAME = "Export.xlsx";
	private static final long DEFAULT_COUNT_RESULT = 0;

	private final Window owner;
	protected ToolBar toolBar;
	protected TableView<T> tableView;
	protected Action refreshAction;
	protected Action exportAction;
	protected Action suspendAction;
	protected Action copyCellAction;
	protected ContextMenu mnuTableView;
	protected StatusBar statusBar;
	protected FormatDTO userFormat = FormatPreferencesManager.getFormatDTO();
	protected DateTimeFormatter dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat())
			.withZone(ZoneId.systemDefault());
	protected DateTimeFormatter dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat())
			.withZone(ZoneId.systemDefault());
	protected DecimalFormat decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
	private DataFetchTask refreshViewTask;

	/**
	 * Constructor
	 * @param owner
	 */
	protected AbstractDataGridPanel(Window owner) {
		this.owner = owner;
	}

	/**
	 * @return the owner window
	 */
	public Window getOwner() {
		return owner;
	}

	/**
	 * @param element
	 * @param colIndex
	 * @return the text of a given cell
	 */
	protected abstract String getCellText(T element, int colIndex);

	/**
	 * Initialize the columns
	 * @return the columns
	 */
	protected abstract ObservableList<TableColumn<T, String>> initColumns();

	/**
	 * @return a list of all elements to be displayed
	 * @throws Exception if the data fetch operation has failed
	 */
	protected abstract List<T> fetchData() throws Exception;

	/**
	 * Every non-abstract subclass must provide a logger instance!
	 * @return the logger
	 */
	protected abstract Logger getLogger();

	/**
	 * Callback method that is called as soon as a data fetch operation starts
	 */
	protected void onStartSearch() {
		// Remove all items from the table view as soon as the data fetch operation starts
		tableView.getItems().clear();

		statusBar.showProgress();
		statusBar.setText(getTranslation(DATA_FETCH_ACTION_STATUS_FETCH_DATA));

		suspendAction.setEnabled(true);
		refreshAction.setEnabled(false);
		exportAction.setEnabled(false);
	}

	/**
	 * Callback method that is called as soon as a data fetch operation is finished
	 * @param itemsFetched
	 * @param countResult
	 * @param timeElapsed
	 */
	@SuppressWarnings("unused")
	protected void onFinishSearch(int itemsFetched, long countResult, long timeElapsed) {
		suspendAction.setEnabled(false);
		refreshAction.setEnabled(true);
		exportAction.setEnabled(true);

		final var params = new ArrayList<>();
		params.add(itemsFetched);
		params.add(String.format("%.2f", timeElapsed / 1000.0));

		final String statusMessage = getTranslation(DATA_FETCH_ACTION_RESULT_NO_COUNT, params.toArray());

		statusBar.stopProgress();
		statusBar.setText(statusMessage);
	}

	/**
	 * Callback method that is called as soon as a data fetch operation has failed
	 * @param cause the exception that caused the data fetch operation to fail. If null the operation has been cancelled by the
	 *          user!
	 */
	protected void onSearchFailed(Throwable cause) {
		statusBar.stopProgress();

		suspendAction.setEnabled(false);
		refreshAction.setEnabled(true);
		exportAction.setEnabled(true);

		if (cause == null)
			statusBar.setText(getTranslation(DATA_FETCH_ACTION_STATUS_OP_CANCELED));
		else
			DialogUtil.openErrorDialog(owner, getTranslation(DATA_FETCH_ACTION_MSG_QUERY_FAILED), cause);
	}

	/**
	 * Callback method that is called as soon as a count operation can be invoked
	 * @return the count result
	 * @throws Exception if the operation has failed
	 */
	protected long onPerformCountOperation() throws Exception {
		return DEFAULT_COUNT_RESULT;
	}

	/**
	 * Initialize the component
	 */
	public void initialize() {
		getLogger().debug("Initialize grid panel");

		toolBar = new ToolBar();
		mnuTableView = new ContextMenu();
		statusBar = new StatusBar();

		tableView = new TableView<>();
		tableView.setContextMenu(mnuTableView);
		tableView.getSelectionModel().setSelectionMode(SelectionMode.MULTIPLE);
		tableView.getSelectionModel().setCellSelectionEnabled(true);
		tableView.setPrefHeight(100);
		tableView.setMaxHeight(Double.MAX_VALUE);
		tableView.setMaxWidth(Double.MAX_VALUE);

		tableView.setOnMouseClicked(e -> {
			if (e.getClickCount() == 2)
				onDoubleClick(tableView.getSelectionModel().getSelectedItem());
		});

		tableView.setOnKeyPressed(e -> {
			if (e.getCode() == KeyCode.ENTER)
				onEnterPressed(getSelectedItem());
			else if (e.getCode() == KeyCode.DELETE)
				onDeletePressed(getSelectedItem());
		});

		getChildren().add(toolBar);
		getChildren().add(tableView);
		getChildren().add(statusBar);

		setPadding(new Insets(5, 5, 5, 5));

		VBox.setVgrow(tableView, Priority.ALWAYS);

		getLogger().debug("Grid panel initialization finished");
	}

	/**
	 * @return the selected element
	 */
	public T getSelectedItem() {
		return tableView.getSelectionModel().getSelectedItem();
	}

	/**
	 * Add an action to the context menu
	 * @param action
	 */
	protected void addActionToContextMenu(Action action) {
		final MenuItem item = action.createMenuItem();

		if (item == null)
			return;

		mnuTableView.getItems().add(item);
	}

	/**
	 * @param action
	 * @param keyCodeCombination
	 */
	protected void addActionToContextMenu(Action action, KeyCodeCombination keyCodeCombination) {
		final MenuItem item = action.createMenuItem(keyCodeCombination);

		if (item == null)
			return;

		mnuTableView.getItems().add(item);
	}

	/**
	 * Add an action to the toolbar
	 * @param action
	 */
	protected void addActionToToolBar(Action action) {
		final Button button = action.createToolbarButton();

		if (button == null)
			return;

		toolBar.getItems().add(button);
	}

	/**
	 * @return the toolbar
	 */
	public ToolBar getToolBar() {
		return toolBar;
	}

	/**
	 * @return the table view's context menu
	 */
	public ContextMenu getContextMenu() {
		return mnuTableView;
	}

	/**
	 * @return the table view
	 */
	public TableView<T> getTableView() {
		return tableView;
	}

	/**
	 * @return the status bar
	 */
	public StatusBar getStatusBar() {
		return statusBar;
	}

	/**
	 * Refresh the view
	 */
	public void refreshView() {
		new Thread(refreshViewTask = new DataFetchTask()).start();
	}

	/**
	 * Action to refresh the view
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
	 * Action to copy the content of the selected cells to the system clipboard
	 */
	protected class CopyCellAction extends Action {
		/**
		 * Constructor
		 */
		public CopyCellAction() {
			this.title = getTranslation(ACTION_COPY_TITLE);
			this.image = getImage(IMG_COPY);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()
		 */
		@Override
		public void handle() {
			final var clipboardString = new StringBuilder();
			int previousRow = -1;

			for (final TablePosition<?, ?> pos : tableView.getSelectionModel().getSelectedCells()) {
				final int currentRow = pos.getRow();
				final int currentCol = pos.getColumn();

				Object cell = tableView.getColumns().get(currentCol).getCellData(currentRow);

				if (cell == null)
					cell = "";

				if (previousRow == currentRow)
					clipboardString.append('\t');
				else if (previousRow != -1)
					clipboardString.append('\n');

				clipboardString.append(cell);

				previousRow = currentRow;
			}

			final var content = new ClipboardContent();
			content.putString(clipboardString.toString());

			Clipboard.getSystemClipboard().setContent(content);
		}
	}

	/**
	 * Action to export the view content
	 */
	private class ExportAction extends Action {
		/**
		 * Constructor
		 */
		public ExportAction() {
			this.title = getTranslation(ACTION_EXPORT_TITLE);
			this.image = getImage(IMG_EXPORT_EXCEL);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.javafx.control.Action#handle()
		 */
		@Override
		public void handle() {
			final var xlsxExporter = new XLSXExportUtility(tableView);

			if (!Desktop.isDesktopSupported()) {
				final var fc = new FileChooser();
				fc.setTitle(getTranslation(ACTION_EXPORT_DIALOG_TITLE));
				fc.getExtensionFilters().add(new ExtensionFilter(getTranslation(ACTION_EXPORT_XLSX_FILTER_DESC), "*.xlsx"));
				fc.setInitialFileName(DEFAULT_EXPORT_FILE_NAME);

				final File file = fc.showSaveDialog(getParent().getScene().getWindow());

				if (file == null)
					return;

				try {
					xlsxExporter.exportToFile(file);
				}
				catch (final Exception e) {
					getLogger().error("Error while exporting table data!", e);

					DialogUtil.openErrorDialog(owner, getTranslation(ACTION_EXPORT_MSG_EXPORT_ERROR), e);
				}
			}
			else {
				File file;

				try {
					file = xlsxExporter.exportToTempFile();
				}
				catch (final Exception e) {
					getLogger().error("Error while exporting table data!", e);

					DialogUtil.openErrorDialog(owner, getTranslation(ACTION_EXPORT_MSG_EXPORT_ERROR), e);
					return;
				}

				final var exportTask = new Task<Void>() {
					/*
					 * (non-Javadoc)
					 * @see javafx.concurrent.Task#call()
					 */
					@Override
					protected Void call() throws Exception {
						Desktop.getDesktop().open(file);

						return null;
					}
				};

				new Thread(exportTask).start();
			}
		}
	}

	/**
	 * Initialize actions
	 */
	public void initActions() {
		refreshAction = new RefreshAction();
		exportAction = new ExportAction();
		suspendAction = new SuspendSearchAction();
		copyCellAction = new CopyCellAction();

		getContextMenu().getItems().add(refreshAction.createMenuItem());
		getContextMenu().getItems().add(exportAction.createMenuItem());
		getContextMenu().getItems().add(copyCellAction.createMenuItem());

		toolBar.getItems().add(refreshAction.createToolbarButton());
		toolBar.getItems().add(exportAction.createToolbarButton());
		toolBar.getItems().add(suspendAction.createToolbarButton());

		suspendAction.setEnabled(false);
	}

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

			getLogger().debug("Perform data fetch operation");

			Platform.runLater(AbstractDataGridPanel.this::onStartSearch);

			data = fetchData();

			if (isDone())
				return null;

			countResult = onPerformCountOperation();

			if (isDone())
				return null;

			Platform.runLater(() -> {
				if (data != null) {
					// Add the items to the table view
					tableView.setItems(FXCollections.observableArrayList(data));

					onFinishSearch(data.size(), countResult, System.currentTimeMillis() - start);
				}
				else
					onFinishSearch(0, countResult, System.currentTimeMillis() - start);

				getLogger().debug("Data fetch operation finished");
			});

			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see javafx.concurrent.Task#failed()
		 */
		@Override
		protected void failed() {
			getLogger().error("Error while fetching data!", getException());

			onSearchFailed(getException());
		}
	}

}
