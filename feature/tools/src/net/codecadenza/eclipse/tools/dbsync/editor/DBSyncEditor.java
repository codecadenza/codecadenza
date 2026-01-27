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
package net.codecadenza.eclipse.tools.dbsync.editor;

import static net.codecadenza.eclipse.shared.Constants.IMG_CLIENT_CHECK;
import static net.codecadenza.eclipse.shared.Constants.IMG_CLIENT_UNCHECK;
import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN_DELETE;
import static net.codecadenza.eclipse.shared.Constants.IMG_COLUMN_EDIT;
import static net.codecadenza.eclipse.shared.Constants.IMG_INDEX_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_INDEX_DELETE;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY_DELETE;
import static net.codecadenza.eclipse.shared.Constants.IMG_PERFORM_SYNC;
import static net.codecadenza.eclipse.shared.Constants.IMG_REFRESH;
import static net.codecadenza.eclipse.shared.Constants.IMG_TABLE_NEW;

import java.lang.reflect.InvocationTargetException;
import java.util.List;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.CodeCadenzaToolsPlugin;
import net.codecadenza.eclipse.tools.dbsync.dialog.EditDDLTransactionDialog;
import net.codecadenza.eclipse.tools.dbsync.model.DDLTransaction;
import net.codecadenza.eclipse.tools.dbsync.model.DDLTransactionState;
import net.codecadenza.eclipse.tools.dbsync.model.DDLTransactionType;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.viewers.CellEditor;
import org.eclipse.jface.viewers.CheckboxCellEditor;
import org.eclipse.jface.viewers.ICellModifier;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.MouseTrackAdapter;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.TableItem;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.part.EditorPart;

/**
 * <p>
 * Editor for database synchronization
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DBSyncEditor extends EditorPart {
	public static final String ID = "net.codecadenza.eclipse.tools.dbsync.editor.DBSyncEditor";
	private static final String COL_MESSAGE = "Message";
	private static final String COL_DDL = "DDL statement";
	private static final String COL_STATE = "State";
	private static final String COL_TYPE = "Type";
	private static final String COL_EXECUTE = "Execute";
	private static final String COL_TRANSACTION_ID = "Transaction ID";

	private Table table;
	private Tree tree;
	private List<DDLTransaction> transactions;
	private Datasource ds;
	private Database db;
	private TableViewer tableViewer;
	private Shell thisShell;
	private IWorkbenchWindow workbenchWindow;
	private ToolItem itemSynchronize;
	private ToolItem itemRefresh;

	/**
	 * Table content provider
	 */
	class ContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		public Object[] getElements(Object inputElement) {
			return transactions.toArray();
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
	class TableLabelProvider extends LabelProvider implements ITableLabelProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnText(java.lang.Object, int)
		 */
		@Override
		public String getColumnText(Object element, int columnIndex) {
			final var transaction = (DDLTransaction) element;

			switch (columnIndex) {
				case 0:
					return Integer.toString(transaction.getId());
				case 1:
					return "";
				case 2:
					return transaction.getType().toString();
				case 3:
					return transaction.getState().toString();
				case 4:
					return transaction.getSql();
				case 5:
					return transaction.getErrorMessage();
			}

			return "";
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ITableLabelProvider#getColumnImage(java.lang.Object, int)
		 */
		@Override
		public Image getColumnImage(Object element, int columnIndex) {
			final var transaction = (DDLTransaction) element;

			switch (columnIndex) {
				case 1:
					if (transaction.isExecute())
						return CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_CHECK);

					return CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_UNCHECK);
				case 2:
					if (transaction.getType() == DDLTransactionType.ADD_COLUMN)
						return CodeCadenzaResourcePlugin.getImage(IMG_COLUMN_ADD);

					if (transaction.getType() == DDLTransactionType.ADD_FK)
						return CodeCadenzaResourcePlugin.getImage(IMG_KEY_ADD);

					if (transaction.getType() == DDLTransactionType.ADD_INDEX)
						return CodeCadenzaResourcePlugin.getImage(IMG_INDEX_ADD);

					if (transaction.getType() == DDLTransactionType.ADD_UK)
						return CodeCadenzaResourcePlugin.getImage(IMG_KEY_ADD);

					if (transaction.getType() == DDLTransactionType.CREATE_TABLE)
						return CodeCadenzaResourcePlugin.getImage(IMG_TABLE_NEW);

					if (transaction.getType() == DDLTransactionType.MOD_COLUMN)
						return CodeCadenzaResourcePlugin.getImage(IMG_COLUMN_EDIT);

					if (transaction.getType() == DDLTransactionType.REMOVE_COLUMN)
						return CodeCadenzaResourcePlugin.getImage(IMG_COLUMN_DELETE);

					if (transaction.getType() == DDLTransactionType.REMOVE_FK)
						return CodeCadenzaResourcePlugin.getImage(IMG_COLUMN_DELETE);

					if (transaction.getType() == DDLTransactionType.REMOVE_INDEX)
						return CodeCadenzaResourcePlugin.getImage(IMG_INDEX_DELETE);

					if (transaction.getType() == DDLTransactionType.REMOVE_UK)
						return CodeCadenzaResourcePlugin.getImage(IMG_KEY_DELETE);

					break;
				case 3:
					if (transaction.getState() == DDLTransactionState.OPEN)
						return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_WARN_TSK);
					else if (transaction.getState() == DDLTransactionState.FINISHED
							|| transaction.getState() == DDLTransactionState.SKIPPED)
						return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_INFO_TSK);
					else if (transaction.getState() == DDLTransactionState.ERROR)
						return PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJS_ERROR_TSK);

					break;
			}

			return null;
		}
	}

	/**
	 * Cell modifier
	 */
	class CellModifier implements ICellModifier {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ICellModifier#canModify(java.lang.Object, java.lang.String)
		 */
		@Override
		public boolean canModify(Object element, String property) {
			return property.equals(COL_EXECUTE);
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ICellModifier#getValue(java.lang.Object, java.lang.String)
		 */
		@Override
		public Object getValue(Object element, String property) {
			final var transaction = (DDLTransaction) element;

			return transaction.isExecute();
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.ICellModifier#modify(java.lang.Object, java.lang.String, java.lang.Object)
		 */
		@Override
		public void modify(Object element, String property, Object value) {
			final var item = (TableItem) element;
			final var transaction = (DDLTransaction) item.getData();
			final var execute = (Boolean) value;

			transaction.setExecute(execute);

			tableViewer.update(transaction, null);
		}
	}

	/**
	 * Refresh the editor
	 */
	private void doRefresh() {
		// Remove all items from the tree
		tree.removeAll();
		tableViewer.setInput(null);
		itemRefresh.setEnabled(false);
		itemSynchronize.setEnabled(false);

		final var job = new Job("Check existing database...") {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.core.runtime.jobs.Job#run(org.eclipse.core.runtime.IProgressMonitor)
			 */
			@Override
			protected IStatus run(IProgressMonitor monitor) {
				// Compare the target database with database meta-data in order to synchronize them
				transactions = new DBSynchService(db, ds).compareDataBase(thisShell, monitor, tree);

				monitor.done();

				thisShell.getDisplay().syncExec(() -> {
					itemRefresh.setEnabled(true);
					itemSynchronize.setEnabled(true);
					tableViewer.setInput(transactions);
				});

				return Status.OK_STATUS;
			}
		};

		job.schedule();
	}

	/**
	 * Synchronize the database
	 */
	private void synchDatabase() {
		try {
			workbenchWindow.run(true, false, monitor -> {
				// Begin a new task
				monitor.beginTask("Synchronize...", transactions.size());

				final var syncService = new DBSynchService(db, ds);

				// Perform the transactions
				transactions.forEach(transaction -> {
					monitor.subTask("Run transaction " + transaction.getId());
					monitor.worked(1);

					try {
						if (transaction.isExecute()) {
							syncService.executeDDL(transaction.getSql());
							transaction.setState(DDLTransactionState.FINISHED);
						}
						else
							transaction.setState(DDLTransactionState.SKIPPED);
					}
					catch (final Exception e) {
						transaction.setState(DDLTransactionState.ERROR);
						transaction.setErrorMessage(e.getMessage());

						CodeCadenzaToolsPlugin.getInstance().logInfo(null, e);
					}

					thisShell.getDisplay().asyncExec(() -> {
						try {
							tableViewer.refresh(transaction);
						}
						catch (final Exception e) {
							CodeCadenzaToolsPlugin.getInstance().logError(e);
						}
					});
				});

				monitor.done();
			});
		}
		catch (final InvocationTargetException ex) {
			CodeCadenzaToolsPlugin.getInstance().logError(ex);
		}
		catch (final InterruptedException _) {
			Thread.currentThread().interrupt();

			CodeCadenzaToolsPlugin.getInstance().logInfo("Operation has been interrupted!");
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.part.WorkbenchPart#createPartControl(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	public void createPartControl(Composite parent) {
		final var input = (DBSynchEditorInput) getEditorInput();

		final var panEditorArea = new Composite(parent, SWT.NONE);
		panEditorArea.setLayout(new GridLayout());

		final var toolBar = new ToolBar(panEditorArea, SWT.NONE);

		itemSynchronize = new ToolItem(toolBar, SWT.PUSH);
		itemSynchronize.setImage(CodeCadenzaResourcePlugin.getImage(IMG_PERFORM_SYNC));
		itemSynchronize.setToolTipText("Synchronize");

		itemSynchronize.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				synchDatabase();
			}
		});

		itemRefresh = new ToolItem(toolBar, SWT.PUSH);
		itemRefresh.setToolTipText("Refresh view");
		itemRefresh.setImage(CodeCadenzaResourcePlugin.getImage(IMG_REFRESH));

		itemRefresh.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				doRefresh();
			}
		});

		final var sashForm = new SashForm(panEditorArea, SWT.NONE);
		sashForm.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		tree = new Tree(sashForm, SWT.BORDER);

		tableViewer = new TableViewer(sashForm, SWT.FULL_SELECTION | SWT.BORDER);
		tableViewer.setContentProvider(new ContentProvider());
		tableViewer.setLabelProvider(new TableLabelProvider());
		tableViewer.setCellModifier(new CellModifier());

		tableViewer.addDoubleClickListener(_ -> {
			boolean readonly = true;

			final var sel = (IStructuredSelection) tableViewer.getSelection();

			if (sel == null)
				return;

			final var transaction = (DDLTransaction) sel.getFirstElement();

			if (transaction.getState() == DDLTransactionState.OPEN)
				readonly = false;

			final var dlg = new EditDDLTransactionDialog(thisShell, transaction.getSql(), readonly);
			final int state = dlg.open();

			if (state == Dialog.OK) {
				transaction.setSql(dlg.getStatement());
				tableViewer.refresh();
			}
		});

		table = tableViewer.getTable();
		table.setHeaderVisible(true);

		table.addMouseTrackListener(new MouseTrackAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseTrackAdapter#mouseHover(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseHover(final MouseEvent e) {
				final TableItem item = table.getItem(new Point(e.x, e.y));

				if (item == null || item.getData() == null)
					return;

				final var trans = (DDLTransaction) item.getData();
				table.setToolTipText(trans.getSql());
			}
		});

		// We need an array with two editors as JFace searches for the proper cell editor by using the column index!
		final var editors = new CellEditor[2];
		editors[1] = new CheckboxCellEditor(table);

		final var columnHeader = new String[6];

		final var colTransID = new TableColumn(table, SWT.NONE);
		colTransID.setWidth(120);
		colTransID.setText(COL_TRANSACTION_ID);
		columnHeader[0] = colTransID.getText();

		final var colExec = new TableColumn(table, SWT.NONE);
		colExec.setWidth(80);
		colExec.setText(COL_EXECUTE);
		columnHeader[1] = colExec.getText();

		final var colType = new TableColumn(table, SWT.NONE);
		colType.setWidth(140);
		colType.setText(COL_TYPE);
		columnHeader[2] = colType.getText();

		final var colState = new TableColumn(table, SWT.NONE);
		colState.setWidth(120);
		colState.setText(COL_STATE);
		columnHeader[3] = colState.getText();

		final var colSQL = new TableColumn(table, SWT.NONE);
		colSQL.setWidth(400);
		colSQL.setText(COL_DDL);
		columnHeader[4] = colSQL.getText();

		final var colMessage = new TableColumn(table, SWT.NONE);
		colMessage.setWidth(300);
		colMessage.setText(COL_MESSAGE);
		columnHeader[5] = colMessage.getText();

		final var menuTable = new Menu(table);
		table.setMenu(menuTable);

		final var menuItemSelectAll = new MenuItem(menuTable, SWT.NONE);
		menuItemSelectAll.setText("Select all");

		menuItemSelectAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				super.widgetSelected(e);

				transactions.forEach(t -> t.setExecute(true));

				tableViewer.refresh();
			}
		});

		final var menuItemUnselectAll = new MenuItem(menuTable, SWT.NONE);
		menuItemUnselectAll.setText("Unselect all");

		menuItemUnselectAll.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				super.widgetSelected(e);

				transactions.forEach(t -> t.setExecute(false));

				tableViewer.refresh();
			}
		});

		final var menuItemCopyAllSelected = new MenuItem(menuTable, SWT.NONE);
		menuItemCopyAllSelected.setText("Copy all selected");

		menuItemCopyAllSelected.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				super.widgetSelected(e);

				final var clipboardString = new StringBuilder();

				for (final DDLTransaction transaction : transactions) {
					if (!transaction.isExecute())
						continue;

					clipboardString.append(transaction.getSql());
					clipboardString.append(";");
					clipboardString.append(System.lineSeparator());
				}

				final var clipboard = new Clipboard(Display.getCurrent());
				clipboard.clearContents();

				if (!clipboardString.isEmpty())
					clipboard.setContents(new Object[] { clipboardString.toString() }, new Transfer[] { TextTransfer.getInstance() });

				clipboard.dispose();
			}
		});

		tableViewer.setCellEditors(editors);

		// In JFace this method has to be called otherwise the cell editor does not work!
		tableViewer.setColumnProperties(columnHeader);

		tableViewer.setInput(transactions);

		this.ds = input.getDs();
		this.db = input.getDatabase();

		doRefresh();

		sashForm.setWeights(20, 60);
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
	 * @see org.eclipse.ui.part.EditorPart#init(org.eclipse.ui.IEditorSite, org.eclipse.ui.IEditorInput)
	 */
	@Override
	public void init(IEditorSite site, IEditorInput input) throws PartInitException {
		this.setSite(site);
		this.setInput(input);
		thisShell = site.getShell();
		workbenchWindow = this.getSite().getWorkbenchWindow();
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
