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
package net.codecadenza.eclipse.tools.sqleditor;

import static net.codecadenza.eclipse.shared.Constants.IMG_EXPORT_CSV;
import static net.codecadenza.eclipse.shared.Constants.MODEL_FOLDER;
import static net.codecadenza.eclipse.shared.Constants.MODEL_ROOT_FILE;
import static net.codecadenza.eclipse.shared.Constants.PREF_DATE_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.PREF_TIME_FORMAT;

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.sql.Connection;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import net.codecadenza.eclipse.tools.sqleditor.actions.SQLEditorToolBar;
import net.codecadenza.eclipse.tools.sqleditor.export.ExportFactory;
import net.codecadenza.eclipse.tools.sqleditor.export.ExportTypeEnumeration;
import net.codecadenza.eclipse.tools.sqleditor.util.SQLSyntaxInitializer;
import net.codecadenza.eclipse.tools.sqleditor.viewer.SQLSyntax;
import net.codecadenza.eclipse.tools.util.db.DBManager;
import net.codecadenza.eclipse.tools.util.db.DBQueryResultSet;
import net.codecadenza.eclipse.tools.util.db.DBResultFormatException;
import net.codecadenza.eclipse.tools.util.db.DatabaseQueryUtil;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.emf.common.util.URI;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.impl.ResourceSetImpl;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableColorProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.KeyAdapter;
import org.eclipse.swt.events.KeyEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.FileDialog;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.EditorPart;
import org.eclipse.ui.part.FileEditorInput;

/**
 * <p>
 * SQL editor
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SQLEditor extends EditorPart {
	public static final String ID = "net.codecadenza.eclipse.tools.sqleditor.SQLEditor";
	private static final String KEYWORD_SELECT = "select";
	private static final String DLG_TITLE_EXPORT = "Data export";
	private static final String COMMAND_DELIMITER = ";";

	private SQLEditorToolBar toolBar;
	private SQLTextEditor textEditor;
	private boolean isDirty;
	private Table tableResults;
	private DBQueryResultSet rs;
	private TableViewer tableViewerResults;
	private TableViewer tableViewerMessages;
	private Shell shell;
	private MenuItem mnuExportGroup;
	private Project project;
	private SQLSyntax syntax;
	private String currentSelectedCellValue = "";
	private Connection connection;
	private final ArrayList<Message> messages = new ArrayList<>();
	private DBManager dbManager;

	/**
	 * Enumeration of message states
	 */
	private enum MessageState {
		OK, ERROR
	}

	/**
	 * Value object for messages
	 */
	private class Message {
		protected String sql;
		protected String text;
		protected int no;
		protected MessageState state;
		protected long duration;

		/**
		 * Constructor
		 * @param startTime
		 * @param state
		 * @param sql
		 * @param text
		 * @param numberOfProcessedObjects
		 */
		protected Message(long startTime, MessageState state, String sql, String text, int numberOfProcessedObjects) {
			this.state = state;
			this.sql = sql;
			this.duration = System.currentTimeMillis() - startTime;
			this.no = messages.size() + 1;

			if (text == null || text.isEmpty()) {
				if (numberOfProcessedObjects == -1)
					this.text = "Query executed sucessfully!";
				else
					this.text = "Number of objects that has been processed: " + numberOfProcessedObjects;
			}
			else
				this.text = text;
		}

	}

	/**
	 * Content provider for messages
	 */
	private static class MessageContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			return ((ArrayList<Message>) inputElement).toArray();
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
	 * Label provider for messages
	 */
	private static class MessageTableLabelProvider extends LabelProvider implements ITableLabelProvider, ITableColorProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		public String getColumnText(Object element, int columnIndex) {
			final var msg = (Message) element;

			if (columnIndex == 0)
				return Long.toString(msg.no);
			else if (columnIndex == 1)
				return msg.state.name();
			else if (columnIndex == 2)
				return msg.sql;
			else if (columnIndex == 3)
				return msg.text;
			else if (columnIndex == 4)
				return Long.toString(msg.duration);

			return "";
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			final var msg = (Message) element;

			if (columnIndex == 1) {
				if (msg.state == MessageState.OK)
					return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK);

				return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK);
			}

			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getBackground(java.lang.Object, int)
		 */
		@Override
		public Color getBackground(Object element, int columnIndex) {
			return null;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableColorProvider#getForeground(java.lang.Object, int)
		 */
		@Override
		public Color getForeground(Object element, int columnIndex) {
			final var msg = (Message) element;

			if (msg.state == SQLEditor.MessageState.ERROR)
				return Display.getCurrent().getSystemColor(SWT.COLOR_RED);

			return null;
		}
	}

	/**
	 * Content provider for query results
	 */
	private static class ResultContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		public Object[] getElements(Object inputElement) {
			if (inputElement == null)
				return null;

			return ((DBQueryResultSet) inputElement).getRows().toArray();
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
	 * Label provider
	 */
	private static class ResultTableLabelProvider extends LabelProvider implements ITableLabelProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public String getColumnText(Object element, int columnIndex) {
			if (element == null)
				return null;

			if (element instanceof List) {
				final var row = (List<String>) element;

				return row.get(columnIndex);
			}

			return "";
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			return null;
		}
	}

	/**
	 * Add a message to the viewer
	 * @param startTime
	 * @param state
	 * @param sql
	 * @param errorText
	 * @param numberOfProcessedObjects
	 */
	private void addMessage(long startTime, MessageState state, String sql, String errorText, int numberOfProcessedObjects) {
		final var msg = new Message(startTime, state, sql, errorText, numberOfProcessedObjects);

		// Add a message to the list of messages and update the display
		shell.getDisplay().syncExec(() -> {
			messages.add(msg);
			tableViewerMessages.setInput(messages);
			tableViewerMessages.refresh();
		});
	}

	/**
	 * Execute the SQL statement
	 * @param monitor
	 * @param connection
	 * @param sql the SQL command to be executed
	 * @param maxRowCount the max. number of rows to be fetched
	 * @param dateFormat the date format
	 * @param timeFormat the time format
	 */
	private void processSQLStatement(IProgressMonitor monitor, final Connection connection, final String sql, int maxRowCount,
			String dateFormat, String timeFormat) {
		final boolean isSelectQuery = sql.toLowerCase().startsWith(KEYWORD_SELECT);
		final long start = System.currentTimeMillis();
		rs = null;

		monitor.subTask(sql);

		if (isSelectQuery)
			shell.getDisplay().syncExec(() -> {
				tableViewerResults.setInput(null);

				// Remove all existing columns
				for (final TableColumn col : tableViewerResults.getTable().getColumns())
					col.dispose();
			});

		if (isSelectQuery) {
			try {
				rs = DatabaseQueryUtil.executeQuery(connection, sql, maxRowCount, dateFormat, timeFormat);
				addMessage(start, MessageState.OK, sql, null, -1);
			}
			catch (final DBResultFormatException | SQLException e) {
				addMessage(start, MessageState.ERROR, sql, e.getMessage(), -1);
			}
		}
		else {
			try {
				final int numberOfProcessedObjects = DatabaseQueryUtil.executeUpdate(connection, sql);
				addMessage(start, MessageState.OK, sql, null, numberOfProcessedObjects);
			}
			catch (final SQLException e) {
				addMessage(start, MessageState.ERROR, sql, e.getMessage(), -1);
			}
		}

		if (isSelectQuery)
			shell.getDisplay().syncExec(() -> {
				if (rs == null)
					return;

				final var colArray = new ArrayList<TableColumn>();

				// Add the columns
				for (final String colName : rs.getHeader()) {
					final var column = new TableColumn(tableViewerResults.getTable(), SWT.NONE);
					column.setText(colName.toUpperCase());

					colArray.add(column);
				}

				// Change the input
				tableViewerResults.setInput(rs);

				colArray.forEach(TableColumn::pack);
			});

		monitor.worked(1);

	}

	/**
	 * Extract distinct SQL commands that are separated by a specified delimiter
	 * @param sql
	 * @return an array of SQL commands
	 */
	private ArrayList<String> extractSQLCommands(String sql) {
		final String[] commands = sql.split(COMMAND_DELIMITER);
		final var commandList = new ArrayList<String>();

		// Remove all comments
		for (final String command : commands) {
			var newCommand = "";
			boolean isFirstLine = true;

			// Remove all single-line comments
			final String[] commandLines = command.trim().split(System.lineSeparator());

			for (String line : commandLines) {
				line = line.trim();

				if (!line.startsWith("//") && !line.isEmpty()) {
					if (isFirstLine)
						isFirstLine = false;
					else
						newCommand += " ";

					newCommand += line;
				}
			}

			// Remove all multi-line comments
			while (newCommand.contains("/*") && newCommand.contains("*/")) {
				if (newCommand.indexOf("*/") > newCommand.indexOf("/*")) {
					final String multiComment = newCommand.substring(newCommand.indexOf("/*"), newCommand.indexOf("*/") + 2);
					newCommand = newCommand.replace(multiComment, "").trim();
				}
			}

			if (!newCommand.isEmpty())
				commandList.add(newCommand);
		}

		return commandList;
	}

	/**
	 * Read the SQL command from the text editor, extract the SQL statements and execute them
	 */
	public void processAllSQLStatements() {
		final ArrayList<String> commandList = extractSQLCommands(textEditor.getSQLTextViewer().getTextWidget().getText());

		// Get global preferences to be used for queries
		final IPreferenceStore store = CodeCadenzaToolsPlugin.getInstance().getPreferenceStore();
		final String dateFormat = store.getString(PREF_DATE_FORMAT);
		final String timeFormat = store.getString(PREF_TIME_FORMAT);
		final int maxRowCount = toolBar.getLimitResults();
		final boolean autoCommit = toolBar.isAutoCommit();

		toolBar.disableActions();

		try {
			connection.setAutoCommit(autoCommit);
		}
		catch (final Exception _) {
			// This exception will be ignored!
		}

		// Clear all existing messages!
		messages.clear();
		tableViewerMessages.setInput(messages);

		// Clear all items of the result view
		tableViewerResults.setInput(null);

		// Remove all existing columns
		for (final TableColumn col : tableViewerResults.getTable().getColumns())
			col.dispose();

		final var job = new Job("Execute query!") {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
			 */
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				monitor.beginTask("Process SQL statements...", commandList.size());

				// Iterate over all SQL statements
				commandList.forEach(command -> processSQLStatement(monitor, connection, command, maxRowCount, dateFormat, timeFormat));

				shell.getDisplay().syncExec(() -> {
					toolBar.enableActions();
					mnuExportGroup.setEnabled(true);
				});

				monitor.done();

				return Status.OK_STATUS;
			}
		};

		job.schedule();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		Database database = null;

		// Configure the editor
		setSite(site);
		setInput(input);

		final var fileInput = (FileEditorInput) input;

		// Get the project where the file is located
		final IProject proj = fileInput.getFile().getParent().getProject();

		// Get the model file
		final var f = new File(
				proj.getLocation().toString() + File.separatorChar + MODEL_FOLDER + File.separatorChar + MODEL_ROOT_FILE);

		if (f.exists()) {
			// Load the project from the respective file
			final var resourceSet = new ResourceSetImpl();

			final URI namespaceURI = URI.createFileURI(f.getAbsolutePath());

			// Get the resource object
			final Resource projectResource = resourceSet.getResource(namespaceURI, true);

			for (final EObject e : projectResource.getContents()) {
				if (e instanceof final Project aProject) {
					project = aProject;
					database = project.getDatabase();
					break;
				}
			}
		}

		if (database == null)
			throw new PartInitException(
					"The editor could not be initialized! The selected file doesn't belong to a CodeCadenza project!");

		dbManager = new DBManager(project);

		try {
			connection = dbManager.getConnection();
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);
			throw new PartInitException("The database connection could not be established! Message: " + e.getMessage());
		}

		try {
			syntax = SQLSyntaxInitializer.initializeSQLSyntax(database);
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);
			throw new PartInitException("The editor could not be initialized! Message: " + e.getMessage());
		}

		// Create the text editor
		textEditor = new SQLTextEditor(this, syntax);
		textEditor.init(site, input);

		shell = this.getSite().getShell();

		updatePartName();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#dispose()
	 */
	@Override
	public void dispose() {
		if (dbManager != null)
			try {
				dbManager.close();
			}
			catch (final Exception _) {
				// This exception will be ignored!
			}

		super.dispose();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent) {
		parent.setLayout(new FillLayout());

		final var sashFormMain = new SashForm(parent, SWT.NONE);
		sashFormMain.setOrientation(SWT.VERTICAL);

		final var panEditor = new Composite(sashFormMain, SWT.NONE);
		panEditor.setLayout(new GridLayout());

		toolBar = new SQLEditorToolBar(panEditor, this);

		final var panEditorParent = new Composite(panEditor, SWT.NONE);
		panEditorParent.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panEditorParent.setLayout(new FillLayout());

		textEditor.createPartControl(panEditorParent);

		final var sashFormResults = new SashForm(sashFormMain, SWT.NONE);
		sashFormResults.setOrientation(SWT.VERTICAL);

		final var panResultSets = new Composite(sashFormResults, SWT.NONE);
		panResultSets.setLayout(new GridLayout());

		final var lblQueryResults = new Label(panResultSets, SWT.NONE);
		lblQueryResults.setText("Query results:");

		tableViewerResults = new TableViewer(panResultSets, SWT.MULTI | SWT.BORDER);
		tableViewerResults.setContentProvider(new ResultContentProvider());
		tableViewerResults.setLabelProvider(new ResultTableLabelProvider());

		tableResults = tableViewerResults.getTable();
		tableResults.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		tableResults.setLinesVisible(true);
		tableResults.setHeaderVisible(true);

		// Create the context menu
		final var mnuTable = new Menu(tableResults);
		tableResults.setMenu(mnuTable);

		mnuExportGroup = new MenuItem(mnuTable, SWT.CASCADE);
		mnuExportGroup.setText("Export");
		mnuExportGroup.setEnabled(false);

		final var mnuExport = new Menu(mnuExportGroup);
		mnuExportGroup.setMenu(mnuExport);

		final var mnuExportCSV = new MenuItem(mnuExport, SWT.NONE);
		mnuExportCSV.setImage(CodeCadenzaResourcePlugin.getImage(IMG_EXPORT_CSV));
		mnuExportCSV.setText("CSV");

		mnuExportCSV.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				// Get the result set from the viewer
				final var resultSet = (DBQueryResultSet) tableViewerResults.getInput();

				// Create the content to be exported
				final String exportResult = ExportFactory.getDataExporter(ExportTypeEnumeration.CSV_EXPORT).export(resultSet.getHeader(),
						resultSet.getRows());

				// Initialize the dialog to save the export file
				final var dlg = new FileDialog(shell, SWT.SAVE);
				dlg.setFilterExtensions("*.csv");
				dlg.setFileName("export.csv");
				dlg.setText("Save export result in csv-format");

				final String path = dlg.open();

				if (path != null)
					try (final var out = new BufferedWriter(new FileWriter(new File(path)))) {
						out.write(exportResult);

						MessageDialog.openInformation(shell, DLG_TITLE_EXPORT, "Data export finished successfully!");
					}
					catch (final IOException ex) {
						CodeCadenzaToolsPlugin.getInstance().handleInternalError(ex);
					}
			}
		});

		final var mnuItemCopy = new MenuItem(mnuTable, SWT.CASCADE);
		mnuItemCopy.setText("Copy");

		final var mnuCopy = new Menu(mnuItemCopy);
		mnuItemCopy.setMenu(mnuCopy);

		final var mnuCopyCellToClipboard = new MenuItem(mnuCopy, SWT.NONE);
		mnuCopyCellToClipboard.setText("Copy cell value");

		mnuCopyCellToClipboard.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var clipboard = new Clipboard(shell.getDisplay());
				clipboard.clearContents();
				clipboard.setContents(new Object[] { currentSelectedCellValue }, new Transfer[] { TextTransfer.getInstance() });
				clipboard.dispose();
			}
		});

		final var mnuCopySelRowsToClipboard = new MenuItem(mnuCopy, SWT.NONE);
		mnuCopySelRowsToClipboard.setText("Copy selected rows");

		mnuCopySelRowsToClipboard.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TableItem[] selItems = tableResults.getSelection();

				if (selItems == null || selItems.length < 1)
					return;

				final var b = new StringBuilder();

				for (final TableItem item : selItems) {
					for (int i = 0; i < tableResults.getColumnCount(); i++) {
						if (i != 0)
							b.append(SWT.TAB);

						b.append(item.getText(i));
					}

					b.append(System.lineSeparator());
				}

				final var clipboard = new Clipboard(shell.getDisplay());
				clipboard.clearContents();
				clipboard.setContents(new Object[] { b.toString() }, new Transfer[] { TextTransfer.getInstance() });
				clipboard.dispose();
			}
		});

		final var mnuCopyAllRowsToClipboard = new MenuItem(mnuCopy, SWT.NONE);
		mnuCopyAllRowsToClipboard.setText("Copy all rows");

		mnuCopyAllRowsToClipboard.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TableItem[] selItems = tableResults.getItems();

				if (selItems == null || selItems.length < 1)
					return;

				final var b = new StringBuilder();

				for (final TableItem item : selItems) {
					for (int i = 0; i < tableResults.getColumnCount(); i++) {
						if (i != 0)
							b.append(SWT.TAB);

						b.append(item.getText(i));
					}

					b.append(System.lineSeparator());
				}

				final var clipboard = new Clipboard(shell.getDisplay());
				clipboard.clearContents();
				clipboard.setContents(new Object[] { b.toString() }, new Transfer[] { TextTransfer.getInstance() });
				clipboard.dispose();
			}
		});

		// Track the selected cell value
		tableResults.addListener(SWT.MouseDown, event -> {
			final var pt = new Point(event.x, event.y);

			for (final TableItem item : tableResults.getItems())
				for (int i = 0; i < tableResults.getColumnCount(); i++) {
					final Rectangle rect = item.getBounds(i);

					if (rect.contains(pt)) {
						currentSelectedCellValue = item.getText(i);
						return;
					}
				}
		});

		// Copy the cell value to the system clipboard if the user presses 'CTRL+C'
		tableResults.addKeyListener(new KeyAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.KeyAdapter#keyReleased(org.eclipse.swt.events.KeyEvent)
			 */
			@Override
			public void keyReleased(KeyEvent e) {
				if (e.stateMask == SWT.CTRL && e.keyCode != SWT.CTRL && e.keyCode == 99) {
					final var clipboard = new Clipboard(shell.getDisplay());
					clipboard.clearContents();
					clipboard.setContents(new Object[] { currentSelectedCellValue }, new Transfer[] { TextTransfer.getInstance() });
					clipboard.dispose();
				}
			}
		});

		final var panMessages = new Composite(sashFormResults, SWT.NONE);
		panMessages.setLayout(new GridLayout());

		final var lblMessages = new Label(panMessages, SWT.NONE);
		lblMessages.setText("Messages:");

		tableViewerMessages = new TableViewer(panMessages, SWT.MULTI | SWT.BORDER);
		tableViewerMessages.setContentProvider(new MessageContentProvider());
		tableViewerMessages.setLabelProvider(new MessageTableLabelProvider());

		final Table tableMessages = tableViewerMessages.getTable();
		tableMessages.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		tableMessages.setLinesVisible(true);
		tableMessages.setHeaderVisible(true);

		tableViewerMessages.setInput(messages);

		TableColumn col = new TableColumn(tableMessages, SWT.NONE);
		col.setText("No.");
		col.setWidth(40);

		col = new TableColumn(tableMessages, SWT.NONE);
		col.setText("State");
		col.setWidth(60);

		col = new TableColumn(tableMessages, SWT.NONE);
		col.setText("SQL");
		col.setWidth(250);

		col = new TableColumn(tableMessages, SWT.NONE);
		col.setText("Text");
		col.setWidth(350);

		col = new TableColumn(tableMessages, SWT.NONE);
		col.setText("Duration (ms)");
		col.setWidth(80);

		sashFormMain.setWeights(20, 80);
		sashFormResults.setWeights(80, 20);

		// Refresh the text viewer in order to initialize syntax highlighting
		textEditor.getSQLTextViewer().refresh(syntax);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#setInput(org.eclipse.ui.IEditorInput)
	 */
	@Override
	protected void setInput(IEditorInput input) {
		super.setInput(input);

		if (textEditor != null)
			textEditor.setInput(input);

		updatePartName();
	}

	/**
	 * Update the editor part name
	 */
	private void updatePartName() {
		Display.getDefault().syncExec(() -> {
			final String name = getEditorInput().getName();

			setPartName(name);
		});
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void doSave(IProgressMonitor monitor) {
		save(monitor);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#doSaveAs()
	 */
	@Override
	public void doSaveAs() {
		save(null);
	}

	/**
	 * @param monitor
	 * @return always true
	 */
	private boolean save(IProgressMonitor monitor) {
		if (monitor == null)
			monitor = textEditor.getProgressMonitor();

		textEditor.doSave(monitor);
		setIsDirty(textEditor.isDirty());

		return true;
	}

	/**
	 * Commit the transaction
	 */
	public void commitTransaction() {
		try {
			connection.commit();
			toolBar.finishTransaction();
		}
		catch (final SQLException e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);
		}
	}

	/**
	 * Rollback the transaction
	 */
	public void rollbackTransaction() {
		try {
			connection.rollback();
			toolBar.finishTransaction();
		}
		catch (final SQLException e) {
			CodeCadenzaToolsPlugin.getInstance().logError(e);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#isDirty()
	 */
	@Override
	public boolean isDirty() {
		return isDirty || textEditor.isDirty();
	}

	/**
	 * @param isDirty
	 */
	protected void setIsDirty(boolean isDirty) {
		if (this.isDirty != isDirty) {
			this.isDirty = isDirty;
			firePropertyChange(IEditorPart.PROP_DIRTY);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus() {
		textEditor.setFocus();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
	 */
	@Override
	public boolean isSaveAsAllowed() {
		return textEditor.isSaveAsAllowed();
	}

	/**
	 * Clear the text in the editor
	 */
	public void clearText() {
		textEditor.getSQLTextViewer().clearText();
	}

}
