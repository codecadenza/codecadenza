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

import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.ABSTRACT_RESULT_VIEW_STATUS_DEFAULT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_REFRESH;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.CMD_SUSPEND_SEARCH;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_MSG_FETCH_TITLE;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_MSG_QUERY_FAILED;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_RESULT_NO_COUNT;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_STATUS_FETCH_DATA;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_ACTION_STATUS_OP_CANCELED;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.DATA_FETCH_JOB_NAME;
import static net.codecadenza.runtime.richclient.eclipse.i18n.I18NEclipse.getTranslation;

import java.lang.invoke.MethodHandles;
import java.text.DecimalFormat;
import java.text.DecimalFormatSymbols;
import java.time.ZoneId;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.Collection;
import net.codecadenza.runtime.richclient.eclipse.action.AbstractExportXLSXAction;
import net.codecadenza.runtime.richclient.eclipse.image.ImageCache;
import net.codecadenza.runtime.richclient.eclipse.search.util.AbstractColumnSortListener;
import net.codecadenza.runtime.richclient.format.FormatDTO;
import net.codecadenza.runtime.search.dto.SearchDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDTO;
import net.codecadenza.runtime.search.dto.SearchFieldDataTypeEnum;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableFontProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
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
 * Abstract base class to display query results in a view
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 * @param <T> the type of the view
 */
public abstract class __AbstractResultView<T> {
	private static final Logger logger = LoggerFactory.getLogger(MethodHandles.lookup().lookupClass());
	private static final int JOB_IDLE_TIME = 25;

	protected Label lblStatusMessage;
	protected TableViewer tableViewer;
	protected Table table;
	protected Shell parentShell;
	protected Label lblStatusImage;
	protected Display display;
	protected ToolBar toolBar;
	protected ToolBarManager toolBarManager;
	protected Menu popUpMenu;
	protected Collection<T> values;
	protected SearchDTO searchObj;
	protected RefreshAction refreshAction;
	protected SuspendSearchAction suspendAction;
	protected boolean firstSearch = true;
	protected Job queryJob;
	protected FormatDTO userFormat;
	protected DecimalFormat decimalFormat;
	protected DateTimeFormatter dateTimeFormat;
	protected DateTimeFormatter dateFormat;

	/**
	 * @return the search object
	 */
	public SearchDTO getSearchObj() {
		return searchObj;
	}

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
	 * Get the cell image. This method is intended intended to be overwritten by subclasses!
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
	 * Perform the data fetch operation
	 * @return the objects that should be displayed in the view
	 */
	public abstract Collection<T> fetchData();

	/**
	 * @return the format preferences
	 */
	public abstract FormatDTO getFormatPreferences();

	/**
	 * Execute the query
	 */
	protected void executeQuery() {
		final double startTime = System.currentTimeMillis();

		lblStatusMessage.setText(getTranslation(DATA_FETCH_ACTION_STATUS_FETCH_DATA));
		lblStatusImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));

		tableViewer.setInput(new ArrayList<>());

		refreshAction.setEnabled(false);
		suspendAction.setEnabled(true);
		getExportAction().setEnabled(false);

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
							refreshAction.setEnabled(true);
							getExportAction().setEnabled(true);
						});

						// The job should be canceled!
						return Status.CANCEL_STATUS;
					}

					// The loop will be left when the data fetch job is ready!
					if (dataFetchJob.isReady())
						break;
				}

				// If the data fetch job failed the main job should be stopped as it is in the responsibility of the data fetch job to
				// provide an error message to the user!
				if (dataFetchJob.isError())
					return Status.CANCEL_STATUS;

				display.syncExec(() -> {
					if (table.isDisposed())
						return;

					// Set the input
					tableViewer.setInput(values);

					// Measure the execution time
					final double endTime = System.currentTimeMillis();
					final double timeDif = endTime - startTime;
					final int recordCount = values.size();

					final var params = new ArrayList<>();
					params.add(recordCount);
					params.add(String.format("%.2f", timeDif / 1000));

					if (!lblStatusMessage.isDisposed())
						lblStatusMessage.setText(getTranslation(DATA_FETCH_ACTION_RESULT_NO_COUNT, params.toArray()));

					if (!lblStatusImage.isDisposed())
						lblStatusImage.setImage(ImageCache.getImage(ImageCache.IMG_INFO));

					if (!firstSearch) {
						if (table.isDisposed())
							return;

						for (final TableColumn col : table.getColumns())
							col.dispose();

						final var sfds = new SearchFieldDTO[searchObj.getSearchFields().size()];

						for (int i = 0, j = 0; i < sfds.length; i++)
							if (searchObj.getSearchFields().get(i).isVisible())
								sfds[j++] = searchObj.getSearchFields().get(i);

						// Add the columns to the table
						for (final SearchFieldDTO field : searchObj.getSearchFields()) {
							if (!field.isVisible())
								continue;

							if (table.isDisposed())
								return;

							final var col = new TableColumn(table, SWT.NONE);
							col.setWidth(field.getColWidth());
							col.setText(field.getColLabel());

							if (table.isDisposed())
								return;

							col.addListener(SWT.Selection, new ColumnSorter(tableViewer, searchObj, userFormat));
						}

						firstSearch = false;
					}

					if (table.isDisposed())
						return;

					for (final TableColumn col : table.getColumns())
						col.pack();

					table.redraw();

					refreshAction.setEnabled(true);
					suspendAction.setEnabled(false);
					getExportAction().setEnabled(true);
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
	 * Action to suspend the search thread
	 */
	protected class SuspendSearchAction extends Action {
		/**
		 * Constructor
		 */
		public SuspendSearchAction() {
			super("", Action.AS_PUSH_BUTTON);

			this.setToolTipText(getTranslation(CMD_SUSPEND_SEARCH));
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
	 * Job that performs the data fetch operation
	 */
	protected class DataFetchJob extends Job {
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
			}
			catch (final Exception e) {
				logger.error("Error while fetching data!", e);

				display.syncExec(() -> {
					refreshAction.setEnabled(true);
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
	 * Refresh the format settings
	 */
	protected void refreshFormatSettings() {
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
			return searchObj.getSearchFields().stream().filter(s -> s.getColOrder() == columnIndex).findFirst()
					.map(s -> getCellBackground((T) element, s.getOriginalColumnIndex())).orElse(null);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Color getForeground(Object element, int columnIndex) {
			return searchObj.getSearchFields().stream().filter(s -> s.getColOrder() == columnIndex).findFirst()
					.map(s -> getCellForeground((T) element, s.getOriginalColumnIndex())).orElse(null);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Image getColumnImage(Object element, int columnIndex) {
			for (final SearchFieldDTO s : searchObj.getSearchFields()) {
				if (s.getColOrder() == columnIndex) {
					if (s.getDataType() == SearchFieldDataTypeEnum.BOOLEAN) {
						final String colText = getColText((T) element, s.getOriginalColumnIndex());

						if (colText.equals(Boolean.TRUE.toString()))
							return ImageCache.getImage(ImageCache.IMG_CHECKED);
						else if (colText.equals(Boolean.FALSE.toString()))
							return ImageCache.getImage(ImageCache.IMG_UNCHECKED);
					}
					else
						return getCellImage((T) element, s.getOriginalColumnIndex());
				}
			}

			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getColumnText(Object element, int columnIndex) {
			for (final SearchFieldDTO s : searchObj.getSearchFields())
				if (s.getColOrder() == columnIndex) {
					if (s.getDataType() == SearchFieldDataTypeEnum.BOOLEAN)
						return "";

					return getColText((T) element, s.getOriginalColumnIndex());
				}

			return "";
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableFontProvider#getFont(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Font getFont(Object element, int columnIndex) {
			return searchObj.getSearchFields().stream().filter(s -> s.getColOrder() == columnIndex).findFirst()
					.map(s -> getCellFont((T) element, s.getOriginalColumnIndex())).orElse(null);
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
			return __AbstractResultView.this.getColText(object, colIndex);
		}
	}

	/**
	 * Refresh the view
	 */
	public void refreshView() {
		executeQuery();
	}

	/**
	 * Create actions
	 */
	protected void __createActions() {
		refreshAction = new RefreshAction();
		suspendAction = new SuspendSearchAction();
	}

	/**
	 * Initialize the toolbar
	 */
	protected void __initializeToolBar() {
		toolBarManager = new ToolBarManager(toolBar);
		toolBarManager.add(refreshAction);
		toolBarManager.add(suspendAction);
	}

	/**
	 * Initialize the component
	 * @param parent
	 * @param parentShell
	 */
	public void init(Composite parent, Shell parentShell) {
		final var panMain = new Composite(parent, SWT.NONE);
		panMain.setLayout(new GridLayout());

		this.parentShell = parentShell;
		this.display = parentShell.getDisplay();

		userFormat = getFormatPreferences();
		decimalFormat = new DecimalFormat(userFormat.getDecimalFormat());
		dateTimeFormat = DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(ZoneId.systemDefault());
		dateFormat = DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(ZoneId.systemDefault());

		toolBar = new ToolBar(panMain, SWT.FLAT | SWT.RIGHT);
		toolBar.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		// Create and initialize the table viewer
		tableViewer = new TableViewer(panMain, SWT.FULL_SELECTION | SWT.BORDER);
		tableViewer.setLabelProvider(new TableLabelProvider());
		tableViewer.setContentProvider(new ContentProvider());

		table = tableViewer.getTable();
		table.setLinesVisible(true);
		table.setHeaderVisible(true);
		table.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		popUpMenu = new Menu(table);
		table.setMenu(popUpMenu);

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
		lblStatusMessage.setText(getTranslation(ABSTRACT_RESULT_VIEW_STATUS_DEFAULT));

		initSearch();

		int i = 0;

		for (final SearchFieldDTO field : searchObj.getSearchFields()) {
			if (!field.isVisible()) {
				for (final SearchFieldDTO s : searchObj.getSearchFields())
					if (s.getColOrder() >= i)
						s.setColOrder(s.getColOrder() - 1);

				continue;
			}

			i++;

			final var col = new TableColumn(table, SWT.NONE);
			col.setWidth(field.getColWidth());
			col.setText(field.getColLabel());
			col.addListener(SWT.Selection, new ColumnSorter(tableViewer, searchObj, userFormat));
		}
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
	 * @return the pop-up menu of the table
	 */
	public Menu getPopUpMenu() {
		return this.popUpMenu;
	}

}
