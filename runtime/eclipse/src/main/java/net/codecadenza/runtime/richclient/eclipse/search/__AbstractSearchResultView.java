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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_SEARCH_RESULT_VIEW_ACTION_NAME_SUSPEND;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_REFRESH;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_MSG_FETCH_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_RESULT_NO_COUNT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_RESULT_WITH_COUNT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_STATUS_FETCH_DATA;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_STATUS_OP_CANCELED;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_JOB_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FETCH_NEXT_PAGE_ACTION_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.FETCH_PREV_PAGE_ACTION_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SAVE_QUERY_ACTION_DEFAULT_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SAVE_QUERY_ACTION_MSG_ENTER_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SAVE_QUERY_ACTION_MSG_MISSING_INPUT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SAVE_QUERY_ACTION_MSG_OVERWRITE_QUERY;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SAVE_QUERY_ACTION_MSG_QUERY_DUPLICATE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SAVE_QUERY_ACTION_MSG_QUERY_SAVE_ERROR;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SAVE_QUERY_ACTION_MSG_SAVE_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SAVE_QUERY_ACTION_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.SEARCH_INPUT_ACTION_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.HashMap;
import net.codecadenza.runtime.richclient.eclipse.action.AbstractExportXLSXAction;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.eclipse.search.util.AbstractColumnSortListener;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.richclient.search.util.DuplicateSearchNameException;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IInputValidator;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ControlAdapter;
import org.eclipse.swt.events.ControlEvent;
import org.eclipse.swt.events.DisposeEvent;
import org.eclipse.swt.events.DisposeListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.ToolBar;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

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
public abstract class __AbstractSearchResultView<T> implements Countable {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	public static final String SAVED_QUERY_ID_KEY = "SAVED_SEARCH_ID";
	private static final int JOB_IDLE_TIME = 50;

	protected Display display;
	protected Shell parentShell;
	protected Menu contextMenu;
	protected Label lblStatusImage;
	protected Label lblStatusMessage;
	protected ToolBar toolBar;
	protected ToolBarManager toolBarManager;
	protected TableViewer tableViewer;
	protected Table table;
	protected RefreshAction refreshAction;
	protected SearchInputAction searchAction;
	protected FetchNextPageAction nextAction;
	protected FetchPreviousPageAction previousAction;
	protected SaveQueryAction saveAction;
	protected SuspendSearchAction suspendAction;
	protected Job queryJob;
	protected SearchDTO searchObj;
	protected Collection<T> values;
	protected int pageIndex;
	protected long pageCount;
	protected boolean enableNext;
	protected boolean enablePrevious;
	protected boolean isUserDefQuery;
	protected boolean lastSearchFound = true;
	protected int savedSearchId;
	protected boolean firstSearch = true;
	protected __AbstractSearchResultView<?> thisView;
	protected long totalCount;
	protected int[] tableColOrder;
	protected int lastSwitchColumnIndex;
	protected HashMap<String, ControlAdapter> controlMap = new HashMap<>();
	protected HashMap<String, TableColumn> columnMap = new HashMap<>();
	protected FormatDTO userFormat;
	protected DecimalFormat decimalFormat;
	protected DateTimeFormatter dateTimeFormat;
	protected DateTimeFormatter dateFormat;

	/**
	 * Get the cell background. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell background color
	 */
	@SuppressWarnings("unused")
	public Color getCellBackground(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell foreground color. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell foreground color
	 */
	@SuppressWarnings("unused")
	public Color getCellForeground(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell font. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the font
	 */
	@SuppressWarnings("unused")
	public Font getCellFont(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the cell image. This method is intended to be overwritten by subclasses!
	 * @param element
	 * @param columnIndex
	 * @return the cell image
	 */
	@SuppressWarnings("unused")
	public Image getCellImage(T element, int columnIndex) {
		return null;
	}

	/**
	 * Get the column text
	 * @param element
	 * @param columnIndex
	 * @return the text to be displayed
	 */
	public abstract String getColText(T element, int columnIndex);

	/**
	 * Initialize the search object
	 */
	public abstract void initSearch();

	/**
	 * @return the format preferences
	 */
	public abstract FormatDTO getFormatPreferences();

	/**
	 * Perform the data fetch operation! Note that this method is executed in a different thread!
	 * @return the objects that should be displayed in the view
	 */
	public abstract Collection<T> fetchData();

	/**
	 * @return the search input dialog to be used
	 */
	public abstract __SearchInputDialog getSearchInputDialog();

	/**
	 * @return the ID of the view
	 */
	public abstract String getViewId();

	/**
	 * An implementation of this class must define how to save a search object
	 * @param viewName
	 * @param searchDTO
	 * @param name
	 * @throws DuplicateSearchNameException if a saved search with the same name already exists
	 */
	public abstract void saveSearch(String viewName, SearchDTO searchDTO, String name) throws DuplicateSearchNameException;

	/**
	 * An implementation of this class must define how to save the query that has been performed at last
	 * @param viewName
	 * @param searchDTO
	 */
	public abstract void saveLastSearch(String viewName, SearchDTO searchDTO);

	/**
	 * An implementation of this class must define how to overwrite an existing saved search
	 * @param id
	 * @param searchDTO
	 */
	public abstract void overwriteSavedSearchObject(int id, SearchDTO searchDTO);

	/**
	 * An implementation of this class must define how to find a saved search
	 * @param id
	 * @return the search object
	 */
	public abstract SearchDTO getSavedSearch(int id);

	/**
	 * An implementation of this class must define how to find the search object that has been used at last
	 * @param viewName
	 * @return the search object
	 */
	public abstract SearchDTO getLastSearch(String viewName);

	/**
	 * Execute the query
	 * @param checkTable
	 */
	protected void executeQuery(final boolean checkTable) {
		final double startTime = System.currentTimeMillis();

		lblStatusMessage.setText(getTranslation(DATA_FETCH_ACTION_STATUS_FETCH_DATA));
		lblStatusImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));

		tableViewer.setInput(new ArrayList<>());

		searchAction.setEnabled(false);
		refreshAction.setEnabled(false);
		saveAction.setEnabled(false);
		previousAction.setEnabled(false);
		nextAction.setEnabled(false);
		suspendAction.setEnabled(true);
		getExportAction().setEnabled(false);

		onStartSearch();

		refreshFormatSettings();

		queryJob = new Job(getTranslation(DATA_FETCH_ACTION_STATUS_FETCH_DATA)) {
			/*
			 * (non-Javadoc)
			 * @see java.lang.Runnable#run()
			 */
			@Override
			public IStatus run(IProgressMonitor monitor) {
				logger.debug("Start thread for fetching data");

				// Search the data
				searchObj.setFetchHidden(true);

				final var dataFetchJob = new DataFetchJob();
				dataFetchJob.schedule();

				while (true) {
					// We wait some time until we check both jobs again!
					try {
						Thread.sleep(JOB_IDLE_TIME);
					}
					catch (final InterruptedException e) {
						Thread.currentThread().interrupt();

						logger.warn("Data fetch thread has been interrupted!", e);
					}

					if (monitor.isCanceled()) {
						// If the main job should be canceled the data fetch job should be canceled too!
						dataFetchJob.cancel();

						display.syncExec(() -> {
							if (!lblStatusMessage.isDisposed())
								lblStatusMessage.setText(getTranslation(DATA_FETCH_ACTION_STATUS_OP_CANCELED));

							if (!lblStatusImage.isDisposed())
								lblStatusImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));

							suspendAction.setEnabled(false);
							searchAction.setEnabled(true);
							refreshAction.setEnabled(true);
							saveAction.setEnabled(true);
							previousAction.setEnabled(enablePrevious);
							nextAction.setEnabled(enableNext);
							getExportAction().setEnabled(true);
						});

						// The job should be canceled!
						return Status.CANCEL_STATUS;
					}

					// The loop will be left as soon as the data fetch job is ready!
					if (dataFetchJob.isReady())
						break;
				}

				// If the data fetch job failed the main job should be stopped as it is in the responsibility of the data fetch job to
				// provide an error message to the user!
				if (dataFetchJob.isError())
					return Status.CANCEL_STATUS;

				display.syncExec(() -> {
					String message;

					if (table.isDisposed())
						return;

					// Clear the table items, add the columns and the column sort listeners
					if (checkTable)
						setupTable();

					if (table.isDisposed())
						return;

					// Depending on the amount of data, this operation can take much longer than fetching data from a server!
					tableViewer.setInput(values);

					// Measure the execution time
					final double endTime = System.currentTimeMillis();
					final double timeDif = endTime - startTime;

					onFinishSearch();

					if (searchObj.isCount()) {
						final int maxResult = searchObj.getMaxResult();
						pageCount = totalCount / maxResult;

						if ((totalCount % maxResult) > 0)
							pageCount++;

						if (pageCount == pageIndex || pageCount == 0)
							enableNext = false;
						else
							enableNext = true;

						final var paramList = new ArrayList<>();
						paramList.add(values.size());
						paramList.add(totalCount);
						paramList.add(String.format("%.2f", timeDif / 1000));
						paramList.add(pageCount > 0 ? pageIndex : 0);
						paramList.add(pageCount);

						message = getTranslation(DATA_FETCH_ACTION_RESULT_WITH_COUNT, paramList.toArray());
					}
					else {
						final var paramList = new ArrayList<>();
						paramList.add(values.size());
						paramList.add(String.format("%.2f", timeDif / 1000));

						message = getTranslation(DATA_FETCH_ACTION_RESULT_NO_COUNT, paramList.toArray());

						enableNext = false;
					}

					if (!lblStatusMessage.isDisposed())
						lblStatusMessage.setText(message);

					if (!lblStatusImage.isDisposed())
						lblStatusImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));

					searchAction.setEnabled(true);
					refreshAction.setEnabled(true);
					saveAction.setEnabled(true);
					previousAction.setEnabled(enablePrevious);
					nextAction.setEnabled(enableNext);
					suspendAction.setEnabled(false);
					getExportAction().setEnabled(true);

					if (!isUserDefQuery)
						saveLastSearch(getViewId(), searchObj);
				});

				return Status.OK_STATUS;
			}
		};

		queryJob.schedule();
	}

	/**
	 * Every subclass must provide an implementation for exporting the data to a Microsoft Excel file
	 * @return an export action
	 */
	protected abstract AbstractExportXLSXAction getExportAction();

	/**
	 * @return the selected element
	 */
	@SuppressWarnings("unchecked")
	public T getSelection() {
		final var s = (IStructuredSelection) tableViewer.getSelection();

		if (s == null)
			return null;

		if (s.getFirstElement() == null)
			return null;

		return (T) s.getFirstElement();
	}

	/**
	 * @return the table viewer
	 */
	public TableViewer getTableViewer() {
		return tableViewer;
	}

	/**
	 * @return the table
	 */
	public Table getTable() {
		return table;
	}

	/**
	 * Create a header panel
	 * @param parent
	 */
	@SuppressWarnings("unused")
	public void createHeader(Composite parent) {

	}

	/**
	 * Create a footer panel
	 * @param parent
	 */
	@SuppressWarnings("unused")
	public void createFooter(Composite parent) {

	}

	/**
	 * Callback method that is called as soon as a search operation finished successfully
	 */
	protected void onFinishSearch() {

	}

	/**
	 * Callback method that is called prior to starting a query
	 */
	protected void onStartSearch() {

	}

	/**
	 * Listener for table column dispose events
	 */
	protected class TableColumnDisposeListener implements DisposeListener {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.swt.events.DisposeListener#widgetDisposed(org.eclipse.swt.events.DisposeEvent)
		 */
		@Override
		public void widgetDisposed(DisposeEvent e) {
			controlMap.keySet().forEach(k -> columnMap.get(k).removeControlListener(controlMap.get(k)));

			if (!controlMap.isEmpty())
				controlMap = new HashMap<>();

			columnMap.remove(((TableColumn) e.widget).getText());
		}
	}

	/**
	 * Job that performs the data fetch operation
	 */
	public class DataFetchJob extends Job {
		private boolean ready;
		private boolean error;

		/**
		 * Constructor
		 */
		public DataFetchJob() {
			super(getTranslation(DATA_FETCH_JOB_NAME));

			setSystem(true);
		}

		/**
		 * @return true if the job is ready
		 */
		public boolean isReady() {
			return ready;
		}

		/**
		 * @return true if the data fetch operation has failed
		 */
		public boolean isError() {
			return error;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
		 */
		@Override
		protected IStatus run(IProgressMonitor monitor) {
			try {
				values = fetchData();

				if (monitor.isCanceled()) {
					ready = true;
					return Status.CANCEL_STATUS;
				}

				if (searchObj.isCount()) {
					totalCount = countData();

					if (monitor.isCanceled()) {
						ready = true;
						return Status.CANCEL_STATUS;
					}
				}
			}
			catch (final Exception e) {
				logger.error("Error while fetching data!", e);

				display.syncExec(() -> {
					searchAction.setEnabled(true);
					refreshAction.setEnabled(true);
					saveAction.setEnabled(false);
					previousAction.setEnabled(false);
					nextAction.setEnabled(false);
					suspendAction.setEnabled(false);
					getExportAction().setEnabled(false);

					if (!lblStatusMessage.isDisposed())
						lblStatusMessage.setText(getTranslation(DATA_FETCH_ACTION_MSG_QUERY_FAILED));

					if (!lblStatusImage.isDisposed())
						lblStatusImage.setImage(ImageCache.getImage(ImageCache.IMG_ERROR));

					MessageDialog.openError(parentShell, getTranslation(DATA_FETCH_ACTION_MSG_FETCH_TITLE),
							getTranslation(DATA_FETCH_ACTION_MSG_QUERY_FAILED) + e.getMessage());

					error = true;
				});
			}

			ready = true;
			return Status.OK_STATUS;
		}
	}

	/**
	 * Listener for changing the column order of the respective search fields when a column is moved
	 */
	protected class TableColumnListener extends ControlAdapter {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.swt.events.ControlAdapter#controlMoved(org.eclipse.swt.events.ControlEvent)
		 */
		@Override
		public void controlMoved(ControlEvent e) {
			final var currentOrder = Arrays.copyOf(table.getColumnOrder(), table.getColumnOrder().length);
			boolean changed = false;
			lastSwitchColumnIndex = 0;

			// Test if the table column order has changed
			for (int i = 0; i < tableColOrder.length; i++)
				if (tableColOrder[i] != currentOrder[i]) {
					changed = true;
					lastSwitchColumnIndex = i;
				}

			if (changed) {
				final var switchMap = new HashMap<Integer, Integer>();

				// Change the column orders of the corresponding search fields
				for (int i = 0; i < tableColOrder.length; i++) {
					if (tableColOrder[i] != currentOrder[i] && i <= lastSwitchColumnIndex) {
						// Search for the corresponding search field
						for (final SearchFieldDTO sfd : searchObj.getSearchFields()) {
							if (sfd.getColLabel().equals(table.getColumn(currentOrder[i]).getText())) {
								for (final SearchFieldDTO switchDTO : searchObj.getSearchFields()) {
									if (switchDTO.getColLabel().equals(table.getColumn(tableColOrder[i]).getText())) {
										switchMap.put(sfd.getColOrder(), switchDTO.getColOrder());
										break;
									}
								}

								break;
							}
						}
					}
				}

				for (final SearchFieldDTO sfd : searchObj.getSearchFields())
					if (switchMap.containsKey(sfd.getColOrder())) {
						final int newColOrder = switchMap.get(sfd.getColOrder());
						switchMap.remove(sfd.getColOrder());
						sfd.setColOrder(newColOrder);
					}

				// Set the new column order
				tableColOrder = Arrays.copyOf(currentOrder, currentOrder.length);
			}
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.swt.events.ControlAdapter#controlResized(org.eclipse.swt.events.ControlEvent)
		 */
		@Override
		public void controlResized(ControlEvent e) {
			super.controlResized(e);

			final var column = (TableColumn) e.widget;
			final String name = column.getText();

			for (final SearchFieldDTO sfd : searchObj.getSearchFields())
				if (sfd.getColLabel().equals(name)) {
					sfd.setColWidth(column.getWidth());
					break;
				}
		}
	}

	/**
	 * Refresh the format settings
	 */
	public void refreshFormatSettings() {
		userFormat = getFormatPreferences();
		decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(ZoneId.systemDefault());
		dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(ZoneId.systemDefault());

		if (searchObj == null)
			return;

		searchObj.setDecimalSeparator(DecimalFormatSymbols.getInstance().getDecimalSeparator());
		searchObj.setGroupingSeparator(DecimalFormatSymbols.getInstance().getGroupingSeparator());
		searchObj.setDateFormat(userFormat.getDateFormat());
		searchObj.setDateTimeFormat(userFormat.getDateTimeFormat());
		searchObj.setNumberFormat(userFormat.getDecimalFormat());
	}

	/**
	 * Action to open the search input dialog
	 */
	protected class SearchInputAction extends Action {
		/**
		 * Constructor
		 */
		public SearchInputAction() {
			this.setToolTipText(getTranslation(SEARCH_INPUT_ACTION_NAME));
			this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_SEARCH));
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			refreshFormatSettings();

			// Open the dialog to enter search data
			final __SearchInputDialog dlg = getSearchInputDialog();
			final int returnCode = dlg.open();

			if (returnCode == Dialog.CANCEL)
				return;

			// Get the search object
			searchObj = dlg.getSearchInput();

			// Reset paging
			searchObj.setStartIndex(0);
			pageIndex = 1;

			enableNext = false;
			enablePrevious = false;

			nextAction.setEnabled(enableNext);
			previousAction.setEnabled(enablePrevious);

			executeQuery(true);
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
			this.setToolTipText(getTranslation(FETCH_NEXT_PAGE_ACTION_NAME));
			this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_VIEW_NEXT));
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			if (!enableNext)
				return;

			final int startIndex = searchObj.getStartIndex();
			final int maxResult = searchObj.getMaxResult();

			searchObj.setStartIndex(startIndex + maxResult);
			enablePrevious = true;

			previousAction.setEnabled(enablePrevious);

			pageIndex++;

			executeQuery(false);
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
			this.setToolTipText(getTranslation(FETCH_PREV_PAGE_ACTION_NAME));
			this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_VIEW_PREVIOUS));
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			if (!enablePrevious)
				return;

			final int startIndex = searchObj.getStartIndex();
			final int maxResult = searchObj.getMaxResult();

			searchObj.setStartIndex(startIndex - maxResult);

			if (pageIndex == 2)
				enablePrevious = false;

			enableNext = true;

			previousAction.setEnabled(enablePrevious);
			nextAction.setEnabled(enableNext);

			pageIndex--;
			executeQuery(false);
		}
	}

	/**
	 * Action to save the query
	 */
	protected class SaveQueryAction extends Action {
		/**
		 * Constructor
		 */
		public SaveQueryAction() {
			this.setToolTipText(getTranslation(SAVE_QUERY_ACTION_NAME));
			this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_SAVE_AS));
		}

		/**
		 * Validator that checks if a user has entered a valid name for the saved query
		 */
		private class LengthValidator implements IInputValidator {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.jface.dialogs.IInputValidator#isValid(java.lang.String)
			 */
			@Override
			public String isValid(String newText) {
				// Determine if the input is empty
				if (newText.isEmpty())
					return getTranslation(SAVE_QUERY_ACTION_MSG_MISSING_INPUT);

				// Return null in order to indicate that the input is valid
				return null;
			}
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			final String dialogTitle = getTranslation(SAVE_QUERY_ACTION_MSG_SAVE_TITLE);

			if (isUserDefQuery) {
				final boolean doIt = MessageDialog.openQuestion(parentShell, dialogTitle,
						getTranslation(SAVE_QUERY_ACTION_MSG_OVERWRITE_QUERY));

				if (!doIt)
					return;

				overwriteSavedSearchObject(savedSearchId, searchObj);
			}
			else {
				final String inputDialogMessage = getTranslation(SAVE_QUERY_ACTION_MSG_ENTER_NAME);
				final String inputDialogDefaultValue = getTranslation(SAVE_QUERY_ACTION_DEFAULT_NAME);

				// Open a dialog to enter a name
				final var dlg = new InputDialog(parentShell, dialogTitle, inputDialogMessage, inputDialogDefaultValue,
						new LengthValidator());

				if (dlg.open() == Window.OK)
					try {
						saveSearch(getViewId(), searchObj, dlg.getValue());
					}
					catch (final DuplicateSearchNameException e) {
						MessageDialog.openWarning(parentShell, dialogTitle,
								getTranslation(SAVE_QUERY_ACTION_MSG_QUERY_DUPLICATE, dlg.getValue()));
					}
					catch (final Exception e) {
						logger.error("Error while saving query!", e);

						MessageDialog.openError(parentShell, dialogTitle,
								getTranslation(SAVE_QUERY_ACTION_MSG_QUERY_SAVE_ERROR) + e.getMessage());
					}
			}
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
			super("", Action.AS_PUSH_BUTTON);

			this.setToolTipText(getTranslation(ABSTRACT_SEARCH_RESULT_VIEW_ACTION_NAME_SUSPEND));
			this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_STOP_PROCESS));
			this.setEnabled(false);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			if (queryJob != null)
				queryJob.cancel();
		}
	}

	/**
	 * Action to refresh the view
	 */
	protected class RefreshAction extends Action {
		/**
		 * Constructor
		 */
		public RefreshAction() {
			this.setToolTipText(getTranslation(CMD_REFRESH));
			this.setImageDescriptor(ImageCache.getImageDescriptor(ImageCache.IMG_REFRESH));
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.action.Action#run()
		 */
		@Override
		public void run() {
			refreshView();
		}
	}

	/**
	 * Label provider
	 */
	protected class TableLabelProvider extends LabelProvider
			implements ITableLabelProvider, ITableColorProvider, ITableFontProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getBackground(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Color getBackground(Object element, int columnIndex) {
			return getCellBackground((T) element, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Color getForeground(Object element, int columnIndex) {
			return getCellForeground((T) element, columnIndex);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Image getColumnImage(Object element, int columnIndex) {
			final SearchFieldDTO field = searchObj.getSearchFields().stream().filter(f -> f.getColOrder() == columnIndex).findFirst()
					.orElse(null);

			if (field == null) {
				logger.warn("Search field with column index {} could not be found!", columnIndex);
				return null;
			}

			if (field.getDataType() == SearchFieldDataTypeEnum.BOOLEAN) {
				final String colText = getColText((T) element, field.getOriginalColumnIndex());

				if (colText.equals(Boolean.TRUE.toString()))
					return ImageCache.getImage(ImageCache.IMG_CHECKED);
				else if (colText.equals(Boolean.FALSE.toString()))
					return ImageCache.getImage(ImageCache.IMG_UNCHECKED);
			}

			return getCellImage((T) element, field.getOriginalColumnIndex());
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getColumnText(Object element, int columnIndex) {
			final SearchFieldDTO field = searchObj.getSearchFields().stream().filter(f -> f.getColOrder() == columnIndex).findFirst()
					.orElse(null);

			if (field == null) {
				logger.warn("Search field with column index {} could not be found!", columnIndex);
				return "";
			}

			if (field.getDataType() == SearchFieldDataTypeEnum.BOOLEAN)
				return "";

			return getColText((T) element, field.getOriginalColumnIndex());
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableFontProvider#getFont(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Font getFont(Object element, int columnIndex) {
			return getCellFont((T) element, columnIndex);
		}
	}

	/**
	 * Content provider
	 */
	protected class ContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			return ((Collection<T>) inputElement).toArray();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#dispose()
		 */
		@Override
		public void dispose() {
			// No implementation required!
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IContentProvider#inputChanged(org.eclipse.jface.viewers.Viewer, java.lang.Object,
		 * java.lang.Object)
		 */
		@Override
		public void inputChanged(Viewer viewer, Object oldInput, Object newInput) {
			// No implementation required!
		}
	}

	/**
	 * Class that is responsible to sort columns
	 */
	protected class ColumnSorter extends AbstractColumnSortListener<T> {
		/**
		 * Constructor
		 * @param tableViewer
		 * @param searchObj
		 * @param userFormat
		 */
		public ColumnSorter(TableViewer tableViewer, SearchDTO searchObj, FormatDTO userFormat) {
			super(tableViewer, searchObj, userFormat);
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.runtime.richclient.eclipse.search.util.AbstractColumnSortListener#getColText(java.lang.Object, int)
		 */
		@Override
		public String getColText(T object, int colIndex) {
			return __AbstractSearchResultView.this.getColText(object, colIndex);
		}
	}

	/**
	 * Set the image of a cell
	 * @param value the current value of a cell
	 * @param columnName the name of the column
	 * @return the image of the column
	 */
	@SuppressWarnings("unused")
	public Image setCellImage(String value, String columnName) {
		return null;
	}

	/**
	 * Set the background color of a cell
	 * @param row an array that contains all row values
	 * @param columnIndex the index of the column
	 * @return the color to set
	 */
	@SuppressWarnings("unused")
	public Color setCellBackgroundColor(String[] row, int columnIndex) {
		return null;
	}

	/**
	 * Set the foreground color of a cell
	 * @param row an array that contains all row values
	 * @param columnIndex the index of the column
	 * @return the color to set
	 */
	@SuppressWarnings("unused")
	public Color setCellForegroundColor(String[] row, int columnIndex) {
		return null;
	}

	/**
	 * Set the cell font
	 * @param row an array that contains all row values
	 * @param columnIndex the index of the column
	 * @return the font to set
	 */
	@SuppressWarnings("unused")
	public Font setCellFont(String[] row, int columnIndex) {
		return null;
	}

	/**
	 * Setup the table
	 */
	protected void setupTable() {
		boolean changed = false;
		boolean columnFound = false;

		// Check if the table structure has changed
		for (int j = 0; j < table.getColumnCount(); j++) {
			boolean equal = false;

			for (final SearchFieldDTO field : searchObj.getSearchFields())
				if (table.getColumn(j).getText().equals(field.getColLabel()) && j == field.getColOrder() && field.isVisible()) {
					equal = true;
					break;
				}

			if (!equal) {
				changed = true;
				break;
			}
		}

		// Test if an invisible field becomes visible
		if (!changed) {
			for (final SearchFieldDTO field : searchObj.getSearchFields()) {
				if (!field.isVisible())
					continue;

				columnFound = false;

				for (int j = 0; j < table.getColumnCount(); j++)
					if (table.getColumn(j).getText().equals(field.getColLabel())) {
						columnFound = true;
						break;
					}

				if (!columnFound) {
					changed = true;
					break;
				}
			}
		}

		if (changed) {
			// Remove all columns and rebuild the table
			for (final TableColumn t : table.getColumns())
				t.dispose();

			table = tableViewer.getTable();

			// Sort the field list
			searchObj.getSearchFields().sort((f1, f2) -> f1.getColOrder() - f2.getColOrder());

			// Rebuild the table
			for (final SearchFieldDTO field : searchObj.getSearchFields()) {
				if (!field.isVisible())
					continue;

				final var t = new TableColumn(table, SWT.NONE);
				t.setText(field.getColLabel());
				t.setWidth(field.getColWidth());

				if (field.getDataType() == SearchFieldDataTypeEnum.BOOLEAN)
					t.setAlignment(SWT.CENTER);

				t.addListener(SWT.Selection, new ColumnSorter(tableViewer, searchObj, userFormat));

				final String controlKey = t.getText();

				controlMap.put(controlKey, new TableColumnListener());

				t.addControlListener(controlMap.get(controlKey));
				t.addDisposeListener(new TableColumnDisposeListener());

				columnMap.put(t.getText(), t);
			}

			firstSearch = false;

			// Initialize the table column order
			tableColOrder = Arrays.copyOf(table.getColumnOrder(), table.getColumnOrder().length);
		}

		// Add the column sorters if necessary
		if (firstSearch && !changed) {
			for (int x = 0; x < table.getColumnCount(); x++)
				table.getColumn(x).addListener(SWT.Selection, new ColumnSorter(tableViewer, searchObj, userFormat));

			firstSearch = false;
		}
	}

	/**
	 * Refresh the view
	 */
	public void refreshView() {
		executeQuery(false);
	}

	/**
	 * Create the actions
	 */
	protected void __createActions() {
		refreshAction = new RefreshAction();
		searchAction = new SearchInputAction();
		nextAction = new FetchNextPageAction();
		previousAction = new FetchPreviousPageAction();
		saveAction = new SaveQueryAction();
		suspendAction = new SuspendSearchAction();
	}

	/**
	 * Initialize the toolbar
	 */
	protected void __initializeToolBar() {
		toolBarManager = new ToolBarManager(toolBar);
		toolBarManager.add(refreshAction);
		toolBarManager.add(searchAction);
		toolBarManager.add(saveAction);
		toolBarManager.add(previousAction);
		toolBarManager.add(nextAction);
		toolBarManager.add(suspendAction);
	}

	/**
	 * Initialize the view
	 * @param parent
	 * @param savedSearchId
	 * @param parentShell
	 */
	public void init(Composite parent, Integer savedSearchId, Shell parentShell) {
		final var panMain = new Composite(parent, SWT.NONE);
		panMain.setLayout(new GridLayout());

		// Initialize internal fields
		this.parentShell = parentShell;
		this.display = parentShell.getDisplay();
		this.thisView = this;
		this.searchObj = null;
		this.userFormat = getFormatPreferences();
		this.decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		this.dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(ZoneId.systemDefault());
		this.dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(ZoneId.systemDefault());

		initSearch();

		if (savedSearchId != null) {
			this.isUserDefQuery = true;
			this.savedSearchId = savedSearchId;
			this.searchObj = getSavedSearch(savedSearchId);
		}
		else if (getLastSearch(getViewId()) != null)
			this.searchObj = getLastSearch(getViewId());
		else
			this.lastSearchFound = false;

		toolBar = new ToolBar(panMain, SWT.FLAT | SWT.RIGHT);
		toolBar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		createHeader(panMain);

		tableViewer = new TableViewer(panMain, SWT.BORDER | SWT.FULL_SELECTION);
		tableViewer.setLabelProvider(new TableLabelProvider());
		tableViewer.setContentProvider(new ContentProvider());

		table = tableViewer.getTable();
		table.setLinesVisible(true);
		table.setHeaderVisible(true);
		table.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		createFooter(panMain);

		contextMenu = new Menu(table);
		table.setMenu(contextMenu);

		final var glStatus = new GridLayout(2, false);
		glStatus.marginHeight = 2;
		glStatus.marginWidth = 0;

		final var panStatus = new Composite(panMain, SWT.BORDER);
		panStatus.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		panStatus.setLayout(glStatus);

		lblStatusImage = new Label(panStatus, SWT.NONE);
		lblStatusImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));

		lblStatusMessage = new Label(panStatus, SWT.NONE);
		lblStatusMessage.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, true));

		// Add the columns to the table
		for (final SearchFieldDTO field : searchObj.getSearchFields()) {
			if (!field.isVisible())
				continue;

			final var col = new TableColumn(table, SWT.NONE);

			if (field.getDataType() == SearchFieldDataTypeEnum.BOOLEAN)
				col.setAlignment(SWT.CENTER);

			col.setWidth(field.getColWidth());
			col.setText(field.getColLabel());

			columnMap.put(col.getText(), col);
		}

		// Save the table column order
		tableColOrder = Arrays.copyOf(table.getColumnOrder(), table.getColumnOrder().length);

		// Add column listeners
		for (final TableColumn col : table.getColumns()) {
			col.addDisposeListener(new TableColumnDisposeListener());
			final String controlKey = col.getText();

			controlMap.put(controlKey, new TableColumnListener());
			col.addControlListener(controlMap.get(controlKey));
		}
	}

	/**
	 * @return the search object
	 */
	public SearchDTO getSearchObj() {
		return searchObj;
	}

	/**
	 * @param searchObj the searchObj to set
	 */
	public void setSearchObj(SearchDTO searchObj) {
		this.searchObj = searchObj;
	}

	/**
	 * @return the values
	 */
	public Collection<T> getValues() {
		return values;
	}

	/**
	 * @param values the values to set
	 */
	public void setValues(Collection<T> values) {
		this.values = values;
	}

}
