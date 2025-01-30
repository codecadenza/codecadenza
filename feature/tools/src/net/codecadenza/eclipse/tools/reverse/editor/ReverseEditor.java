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
package net.codecadenza.eclipse.tools.reverse.editor;

import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY;
import static net.codecadenza.eclipse.shared.Constants.IMG_PERFORM_SYNC;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE;
import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.reverse.dialog.ReverseEngineeringConfigDialog;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringConfig;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringLogEntry;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringLogEntry.Source;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringModel;
import net.codecadenza.eclipse.tools.reverse.service.DomainModelValidationService;
import net.codecadenza.eclipse.tools.reverse.service.ReverseEngineeringService;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.EditorPart;

/**
 * <p>
 * Editor for reverse engineering
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReverseEditor extends EditorPart implements ModelChangeListener {
	public static final String ID = ReverseEditor.class.getName();
	public static final int STATE_SUCCESS = 0x200;
	private static final String DLG_TITLE = "Reverse engineering editor";
	private static final int STATUS_COL_INDEX = 0;

	private final ReverseEngineeringConfig configuration = ReverseEngineeringConfig.createDefaultFilter();
	private Tree treeDBModel;
	private Project project;
	private Namespace defaultNamespace;
	private DataGridComposite<ReverseEngineeringLogEntry> tabLogEntries;
	private ReverseEngineeringModel revEngModel;
	private Shell shell;
	private ToolItem itemValidateAndSave;
	private ToolItem itemStartReverseEng;
	private ToolItem itemEditConfig;
	private Database sourceDBModel;
	private Set<String> sequences;
	private DomainModelTreeViewPanel panDomainModelTree;
	private List<ReverseEngineeringLogEntry> reverseEngLog;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent) {
		this.defaultNamespace = ((ReverseEditorInput) getEditorInput()).getNamespace();
		this.project = defaultNamespace.getProject();

		final var panEditorArea = new Composite(parent, SWT.NONE);
		panEditorArea.setLayout(new GridLayout());

		final var toolBar = new ToolBar(panEditorArea, SWT.NONE);

		itemValidateAndSave = new ToolItem(toolBar, SWT.PUSH);
		itemValidateAndSave.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_SAVE_EDIT));
		itemValidateAndSave.setToolTipText("Save domain model");

		itemValidateAndSave.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				validateAndSave();
			}
		});

		itemStartReverseEng = new ToolItem(toolBar, SWT.PUSH);
		itemStartReverseEng.setToolTipText("Start reverse engineering process");
		itemStartReverseEng.setImage(CodeCadenzaResourcePlugin.getImage(IMG_PERFORM_SYNC));

		itemStartReverseEng.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				startReverseEngineeringJob();
			}
		});

		itemEditConfig = new ToolItem(toolBar, SWT.PUSH);
		itemEditConfig.setToolTipText("Edit configuration");
		itemEditConfig.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_ETOOL_CLEAR));

		itemEditConfig.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlg = new ReverseEngineeringConfigDialog(shell, configuration);

				if (Dialog.OK != dlg.open())
					return;

				startReverseEngineeringJob();
			}
		});

		final var sashFormMain = new SashForm(panEditorArea, SWT.NONE);
		sashFormMain.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabFolder = new TabFolder(sashFormMain, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabItemDomainModel = new TabItem(tabFolder, SWT.NONE);
		tabItemDomainModel.setText("Domain model");

		panDomainModelTree = new DomainModelTreeViewPanel(tabFolder, project);
		panDomainModelTree.addModelChangeListener(this);

		final var tabItemDatabaseModel = new TabItem(tabFolder, SWT.NONE);
		tabItemDatabaseModel.setText("Database model");

		treeDBModel = new Tree(tabFolder, SWT.BORDER);

		final var tabFolderDetails = new TabFolder(sashFormMain, SWT.NONE);
		tabFolderDetails.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabItemLogs = new TabItem(tabFolderDetails, SWT.NONE);
		tabItemLogs.setText("Log statements");

		final var panLogs = new Composite(tabFolderDetails, SWT.NONE);
		panLogs.setLayout(new FillLayout());

		tabLogEntries = new DataGridComposite<>(panLogs, SWT.BORDER | SWT.FULL_SELECTION, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(ReverseEngineeringLogEntry element, int columnIndex) {
				if (columnIndex == 0)
					return element.getStatus().name();
				else if (columnIndex == 1)
					return element.getSource().name();
				else if (columnIndex == 2)
					return element.getMessage();

				return "";
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellForeground(java.lang.Object,
			 * int)
			 */
			@Override
			public Color getCellForeground(ReverseEngineeringLogEntry element, int columnIndex) {
				if (element.getStatus() == ReverseEngineeringLogEntry.Status.ERROR)
					return shell.getDisplay().getSystemColor(SWT.COLOR_RED);
				else if (element.getStatus() == ReverseEngineeringLogEntry.Status.WARNING)
					return shell.getDisplay().getSystemColor(SWT.COLOR_DARK_YELLOW);

				return null;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellImage(java.lang.Object, int)
			 */
			@Override
			public Image getCellImage(ReverseEngineeringLogEntry element, int columnIndex) {
				if (columnIndex == 0) {
					if (element.getStatus() == ReverseEngineeringLogEntry.Status.ERROR)
						return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK);
					else if (element.getStatus() == ReverseEngineeringLogEntry.Status.WARNING)
						return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_WARN_TSK);

					return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK);
				}

				return null;
			}
		};

		tabLogEntries.addColumn("Status", 100);
		tabLogEntries.addColumn("Source", 100);
		tabLogEntries.addColumn("Message", ColumnSortType.STRING, 600);

		// Add a special column sorter for the status column
		final TableColumn colStatus = tabLogEntries.getTableViewer().getTable().getColumn(STATUS_COL_INDEX);

		colStatus.addListener(SWT.Selection, event -> {
			final Table table = colStatus.getParent();
			final TableColumn sortColumn = table.getSortColumn();
			int sortDirection = table.getSortDirection();

			if (sortColumn != colStatus) {
				table.setSortColumn(colStatus);
				sortDirection = SWT.UP;
			}
			else
				sortDirection = sortDirection == SWT.UP ? SWT.DOWN : SWT.UP;

			final var logEntries = new ArrayList<>(tabLogEntries.getData());

			displayLogEntries(logEntries, sortDirection);
		});

		tabItemDomainModel.setControl(panDomainModelTree);
		tabItemDatabaseModel.setControl(treeDBModel);
		tabItemLogs.setControl(panLogs);

		sashFormMain.setWeights(1, 2);

		startReverseEngineeringJob();
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.tools.reverse.editor.ModelChangeListener#onModelChanged()
	 */
	@Override
	public void onModelChanged() {
		// Validate the domain model
		final var validationService = new DomainModelValidationService(revEngModel);
		final List<ReverseEngineeringLogEntry> validationLog = validationService.validateDomainModel();

		// Merge both lists
		validationLog.addAll(reverseEngLog);

		// Display import and validation log entries in the table
		displayLogEntries(validationLog, SWT.DOWN);

		final boolean valid = validationLog.stream()
				.filter(log -> log.getSource() == Source.VALIDATION && log.getStatus() != ReverseEngineeringLogEntry.Status.INFO)
				.findFirst().map(log -> false).orElse(true);

		// Disable the save action if the model contains errors or warnings!
		itemValidateAndSave.setEnabled(valid);
	}

	/**
	 * Start the reverse engineering background process
	 */
	private void startReverseEngineeringJob() {
		boolean confirm = true;

		if (revEngModel == null || revEngModel.getDomainObjects().isEmpty() || sourceDBModel == null)
			confirm = false;

		if (confirm) {
			final var msg = "All manual changes get lost when starting a new reverse engineering process! Do you want to continue?";
			final boolean startJob = MessageDialog.openConfirm(shell, DLG_TITLE, msg);

			if (!startJob)
				return;
		}

		// Remove all items from the tree
		treeDBModel.removeAll();

		// Disable user interaction while reading database meta-data!
		itemStartReverseEng.setEnabled(false);
		itemValidateAndSave.setEnabled(false);
		itemEditConfig.setEnabled(false);

		final var job = new Job("Check existing database...") {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
			 */
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				monitor.beginTask("Analyze database structure", IProgressMonitor.UNKNOWN);

				runReverseEngineering();

				monitor.done();

				return Status.OK_STATUS;
			}
		};

		job.schedule();
	}

	/**
	 * Perform the reverse engineering
	 */
	private void runReverseEngineering() {
		final ReverseEngineeringService reverseEngService;

		try {
			// Create a database service and determine the database structure
			final var dbSync = new DBSynchService(project.getDatabase(), project.getDataSource());

			sourceDBModel = dbSync.getDatabaseSnapShot();
			sequences = dbSync.getAllSequences();

			// Infer the domain model from the existing database structure
			reverseEngService = new ReverseEngineeringService(defaultNamespace, sourceDBModel);
			reverseEngService.setSequences(sequences);
			reverseEngService.setConfiguration(configuration);

			revEngModel = reverseEngService.performReverseEngineering();
			reverseEngLog = reverseEngService.getLogEntries();
		}
		catch (final Exception e) {
			shell.getDisplay().syncExec(() -> CodeCadenzaToolsPlugin.getInstance().handleInternalError(e));

			return;
		}

		shell.getDisplay().syncExec(() -> {
			itemStartReverseEng.setEnabled(true);
			itemEditConfig.setEnabled(true);

			panDomainModelTree.setSequences(sequences);
			panDomainModelTree.rebuildTree(revEngModel);

			// Add the tables to the respective tree
			for (final DBTable table : sourceDBModel.getDatabaseTables()) {
				final var tableItem = new TreeItem(treeDBModel, SWT.NONE);
				tableItem.setText(table.getConvertedName());
				tableItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_TABLE));
				tableItem.setData(table);

				// Add the primary key column
				if (table.getPrimaryKey() != null) {
					final DBColumn pkColumn = table.getPrimaryKey().getColumn();
					var colItemText = pkColumn.getConvertedName() + " - " + pkColumn.getColumnType().getName();

					if (pkColumn.getLength() > 0)
						colItemText += "(" + pkColumn.getLength() + ")";

					final var pkColItem = new TreeItem(tableItem, SWT.NONE);
					pkColItem.setText(colItemText);
					pkColItem.setData(pkColumn);
					pkColItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY));
				}

				// Add the columns of the table
				for (final DBColumn col : table.getColumns()) {
					if (table.getPrimaryKey() != null && table.getPrimaryKey().getColumn().equals(col))
						continue;

					var colItemText = col.getConvertedName() + " - " + col.getColumnType().getName();

					if (col.getLength() > 0)
						colItemText += "(" + col.getLength() + ")";

					final var colItem = new TreeItem(tableItem, SWT.NONE);
					colItem.setText(colItemText);
					colItem.setImage(CodeCadenzaResourcePlugin.getImage(IMG_COLUMN));
					colItem.setData(col);

					if (col.isNullable())
						colItem.setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_GRAY));
				}
			}

			onModelChanged();
		});
	}

	/**
	 * Validate and save the results of the reverse engineering operation
	 */
	private void validateAndSave() {
		final ReverseEngineeringService reverseEngService;

		if (revEngModel == null || sourceDBModel == null) {
			final var msg = "The domain model cannot be saved as no reverse engineering configuration is available!";
			MessageDialog.openInformation(shell, DLG_TITLE, msg);
			return;
		}

		// Disable user interaction while performing validation
		itemStartReverseEng.setEnabled(false);
		itemValidateAndSave.setEnabled(false);
		itemEditConfig.setEnabled(false);

		// Validate and save the domain model
		try {
			reverseEngService = new ReverseEngineeringService(defaultNamespace, sourceDBModel);
			reverseEngService.setConfiguration(configuration);
			reverseEngService.setSequences(sequences);

			final List<ReverseEngineeringLogEntry> validationLog = reverseEngService.saveDomainModel(revEngModel);

			// If no validation error has occurred the editor will be closed
			if (validationLog.isEmpty()) {
				// Inform all listeners about a successfully finished reverse engineering operation
				ReverseEditor.this.firePropertyChange(STATE_SUCCESS);

				MessageDialog.openInformation(shell, DLG_TITLE, "Reverse engineering finished successfully!");

				// Close the editor
				PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().closeEditor(ReverseEditor.this, true);
				return;
			}

			itemStartReverseEng.setEnabled(true);
			itemValidateAndSave.setEnabled(true);
			itemEditConfig.setEnabled(true);

			tabLogEntries.setData(validationLog);
		}
		catch (final Exception e) {
			CodeCadenzaToolsPlugin.getInstance().handleInternalError(e);
		}
	}

	/**
	 * @param logEntries
	 * @param sortDirection
	 */
	private void displayLogEntries(List<ReverseEngineeringLogEntry> logEntries, int sortDirection) {
		final TableColumn colStatus = tabLogEntries.getTableViewer().getTable().getColumn(STATUS_COL_INDEX);

		logEntries.sort((logEntry1, logEntry2) -> {
			final ReverseEngineeringLogEntry.Status status1 = logEntry1.getStatus();
			final ReverseEngineeringLogEntry.Status status2 = logEntry2.getStatus();

			// Compare the weighted values
			if (sortDirection == SWT.UP)
				return status1.getWeightedValue() - status2.getWeightedValue();

			return status2.getWeightedValue() - status1.getWeightedValue();
		});

		tabLogEntries.setData(logEntries);
		tabLogEntries.getTableViewer().getTable().setSortDirection(sortDirection);
		tabLogEntries.getTableViewer().getTable().showColumn(colStatus);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#setFocus()
	 */
	@Override
	public void setFocus() {
		// No implementation required!
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#doSave(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	public void doSave(IProgressMonitor monitor) {
		// No implementation required!
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#doSaveAs()
	 */
	@Override
	public void doSaveAs() {
		// No implementation required!
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#dispose()
	 */
	@Override
	public void dispose() {
		firePropertyChange(PROP_DIRTY);

		super.dispose();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		this.setSite(site);
		this.setInput(input);
		this.shell = site.getShell();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#isDirty()
	 */
	@Override
	public boolean isDirty() {
		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.EditorPart#isSaveAsAllowed()
	 */
	@Override
	public boolean isSaveAsAllowed() {
		return false;
	}

}
