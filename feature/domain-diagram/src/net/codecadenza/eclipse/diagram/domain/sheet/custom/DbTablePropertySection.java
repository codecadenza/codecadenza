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
package net.codecadenza.eclipse.diagram.domain.sheet.custom;

import static net.codecadenza.eclipse.shared.Constants.IMG_CLIENT_CHECK;
import static net.codecadenza.eclipse.shared.Constants.IMG_CLIENT_UNCHECK;

import net.codecadenza.eclipse.diagram.domain.dialog.EditDBColumnDialog;
import net.codecadenza.eclipse.diagram.domain.dialog.EditIndexDialog;
import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainObjectEditPart;
import net.codecadenza.eclipse.diagram.domain.edit.parts.ManyToManyAssociationEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorUtil;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.emf.transaction.TransactionalEditingDomain;
import org.eclipse.gmf.runtime.notation.Edge;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CTabFolder;
import org.eclipse.swt.custom.CTabItem;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FillLayout;
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
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property section for database tables
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DbTablePropertySection extends AbstractPropertySection {
	private static final String DLG_TITLE = "Edit table";

	private Table tblColumns;
	private Table tblIndexes;
	private Table tblForeignKeys;
	private TransactionalEditingDomain editingDomain;
	private DBTable table;
	private Text txtPrimaryKeyName;
	private Text txtTableName;
	private Text txtSchemaName;
	private Text txtCatalogName;
	private MenuItem mnuAddUniqueIndex;
	private MenuItem mnuAddIndex;
	private MenuItem mnuDeleteIndex;
	private MenuItem mnuEditIndex;
	private MenuItem mnuFKRename;
	private MenuItem mnuFKDelete;
	private MenuItem mnuEditColumn;
	private ModifyListener textChangeListener;

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls(org.eclipse.swt.widgets.Composite,
	 * org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	@Override
	public void createControls(final Composite parent, TabbedPropertySheetPage tabbedPropertySheetPage) {
		super.createControls(parent, tabbedPropertySheetPage);

		parent.setLayout(new FillLayout(SWT.VERTICAL));

		final Composite panPropertySection = getWidgetFactory().createComposite(parent);

		final var layout = new FillLayout();
		layout.marginWidth = 5;
		layout.marginHeight = 5;

		panPropertySection.setLayout(layout);

		final CTabFolder tabFolder = getWidgetFactory().createTabFolder(panPropertySection, SWT.NONE);

		final CTabItem tabItemGeneral = getWidgetFactory().createTabItem(tabFolder, SWT.NONE);
		tabItemGeneral.setText("General");

		final Composite panGeneral = getWidgetFactory().createComposite(tabFolder);
		panGeneral.setLayout(new GridLayout(2, false));

		final CTabItem tabItemColumns = getWidgetFactory().createTabItem(tabFolder, SWT.NONE);
		tabItemColumns.setText("Columns");

		final Composite panColumns = getWidgetFactory().createComposite(tabFolder);
		panColumns.setLayout(layout);

		final CTabItem tabItemIndexes = getWidgetFactory().createTabItem(tabFolder, SWT.NONE);
		tabItemIndexes.setText("Indexes");

		final Composite panIndexes = getWidgetFactory().createComposite(tabFolder);
		panIndexes.setLayout(layout);

		final CTabItem tabItemKeys = getWidgetFactory().createTabItem(tabFolder, SWT.NONE);
		tabItemKeys.setText("Keys");

		final Composite panKeys = getWidgetFactory().createComposite(tabFolder);
		panKeys.setLayout(layout);

		getWidgetFactory().createCLabel(panGeneral, "Table name:");

		final var gdTableName = new GridData();
		gdTableName.widthHint = 250;

		txtTableName = getWidgetFactory().createText(panGeneral, "");
		txtTableName.setLayoutData(gdTableName);

		getWidgetFactory().createCLabel(panGeneral, "Schema:");

		final var gdSchemaName = new GridData();
		gdSchemaName.widthHint = 250;

		txtSchemaName = getWidgetFactory().createText(panGeneral, "");
		txtSchemaName.setLayoutData(gdSchemaName);

		getWidgetFactory().createCLabel(panGeneral, "Catalog:");

		final var gdCatalogName = new GridData();
		gdCatalogName.widthHint = 250;

		txtCatalogName = getWidgetFactory().createText(panGeneral, "");
		txtCatalogName.setLayoutData(gdCatalogName);

		getWidgetFactory().createCLabel(panGeneral, "Primary key:");

		final var gdPrimaryKeyName = new GridData();
		gdPrimaryKeyName.widthHint = 250;

		txtPrimaryKeyName = getWidgetFactory().createText(panGeneral, "");
		txtPrimaryKeyName.setLayoutData(gdPrimaryKeyName);

		tblColumns = getWidgetFactory().createTable(panColumns, SWT.BORDER);
		tblColumns.setHeaderVisible(true);
		tblColumns.setLinesVisible(true);

		tblColumns.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				editColumn();
			}
		});

		final var colColumnName = new TableColumn(tblColumns, SWT.NONE);
		colColumnName.setWidth(200);
		colColumnName.setText("Name");

		final var colColumnType = new TableColumn(tblColumns, SWT.NONE);
		colColumnType.setWidth(150);
		colColumnType.setText("Type");

		final var colColumnNull = new TableColumn(tblColumns, SWT.NONE);
		colColumnNull.setWidth(100);
		colColumnNull.setText("Nullable");

		final var colColumnPK = new TableColumn(tblColumns, SWT.NONE);
		colColumnPK.setWidth(100);
		colColumnPK.setText("Primary key");

		final var mnuColumn = new Menu(tblColumns);

		mnuEditColumn = new MenuItem(mnuColumn, SWT.NONE);
		mnuEditColumn.setText("Edit");

		mnuEditColumn.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editColumn();
			}
		});

		tblColumns.setMenu(mnuColumn);

		tblIndexes = getWidgetFactory().createTable(panIndexes, SWT.BORDER);
		tblIndexes.setHeaderVisible(true);
		tblIndexes.setLinesVisible(true);

		tblIndexes.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				editIndex();
			}
		});

		final var colIndexName = new TableColumn(tblIndexes, SWT.NONE);
		colIndexName.setWidth(300);
		colIndexName.setText("Name");

		final var colIndexType = new TableColumn(tblIndexes, SWT.NONE);
		colIndexType.setWidth(100);
		colIndexType.setText("Unique");

		final var colIndexCols = new TableColumn(tblIndexes, SWT.NONE);
		colIndexCols.setWidth(300);
		colIndexCols.setText("Columns");

		final var mnuIndexes = new Menu(tblIndexes);

		mnuEditIndex = new MenuItem(mnuIndexes, SWT.NONE);
		mnuEditIndex.setText("Edit");

		mnuEditIndex.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				editIndex();
			}
		});

		mnuAddUniqueIndex = new MenuItem(mnuIndexes, SWT.NONE);
		mnuAddUniqueIndex.setText("Add unique key");

		mnuAddUniqueIndex.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addIndex(true);
			}
		});

		mnuAddIndex = new MenuItem(mnuIndexes, SWT.NONE);
		mnuAddIndex.setText("Add index");

		mnuAddIndex.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				addIndex(false);
			}
		});

		mnuDeleteIndex = new MenuItem(mnuIndexes, SWT.NONE);
		mnuDeleteIndex.setText("Delete index");

		mnuDeleteIndex.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				deleteIndex();
			}
		});

		tblIndexes.setMenu(mnuIndexes);

		tblForeignKeys = getWidgetFactory().createTable(panKeys, SWT.BORDER);
		tblForeignKeys.setLinesVisible(true);
		tblForeignKeys.setHeaderVisible(true);

		final var colFKName = new TableColumn(tblForeignKeys, SWT.NONE);
		colFKName.setWidth(300);
		colFKName.setResizable(true);
		colFKName.setText("Name");

		final var colFKColName = new TableColumn(tblForeignKeys, SWT.NONE);
		colFKColName.setWidth(150);
		colFKColName.setText("Column");

		final var colFKColType = new TableColumn(tblForeignKeys, SWT.NONE);
		colFKColType.setWidth(120);
		colFKColType.setText("Column type");

		final var colFKRefTable = new TableColumn(tblForeignKeys, SWT.NONE);
		colFKRefTable.setWidth(200);
		colFKRefTable.setText("Referenced table");

		final var colFKRefColName = new TableColumn(tblForeignKeys, SWT.NONE);
		colFKRefColName.setWidth(150);
		colFKRefColName.setText("Referenced column");

		final var mnuFK = new Menu(tblForeignKeys);

		mnuFKRename = new MenuItem(mnuFK, SWT.NONE);
		mnuFKRename.setText("Rename");

		mnuFKRename.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				renameForeignKey();
			}
		});

		mnuFKDelete = new MenuItem(mnuFK, SWT.NONE);
		mnuFKDelete.setText("Delete");

		mnuFKDelete.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				deleteForeignKey();
			}
		});

		tblForeignKeys.setMenu(mnuFK);

		tabItemGeneral.setControl(panGeneral);
		tabItemColumns.setControl(panColumns);
		tabItemIndexes.setControl(panIndexes);
		tabItemKeys.setControl(panKeys);

		tabFolder.setSelection(tabItemGeneral);

		textChangeListener = _ -> updateMetaModel();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#setInput(org.eclipse.ui.IWorkbenchPart,
	 * org.eclipse.jface.viewers.ISelection)
	 */
	@Override
	public void setInput(IWorkbenchPart part, ISelection selection) {
		super.setInput(part, selection);

		final Object input = ((IStructuredSelection) selection).getFirstElement();

		if (input instanceof final DomainObjectEditPart editPart) {
			editingDomain = editPart.getEditingDomain();

			final var domainObject = (DomainObject) ((Node) ((DomainObjectEditPart) input).getModel()).getElement();

			table = domainObject.getDatabaseTable();

			if (table == null && domainObject.getParent() != null)
				table = domainObject.getRootParentDomainObject(false).getDatabaseTable();
		}
		else if (input instanceof final ManyToManyAssociationEditPart editPart) {
			editingDomain = editPart.getEditingDomain();
			table = ((ManyToManyAssociation) ((Edge) ((ManyToManyAssociationEditPart) input).getModel()).getElement()).getTable();
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	@Override
	public void refresh() {
		txtTableName.removeModifyListener(textChangeListener);
		txtSchemaName.removeModifyListener(textChangeListener);
		txtCatalogName.removeModifyListener(textChangeListener);
		txtPrimaryKeyName.removeModifyListener(textChangeListener);

		tblColumns.removeAll();
		tblIndexes.removeAll();
		tblForeignKeys.removeAll();

		txtTableName.setEnabled(false);
		txtSchemaName.setEnabled(false);
		txtCatalogName.setEnabled(false);
		txtPrimaryKeyName.setEnabled(false);

		mnuEditIndex.setEnabled(false);
		mnuAddUniqueIndex.setEnabled(false);
		mnuAddIndex.setEnabled(false);
		mnuDeleteIndex.setEnabled(false);
		mnuFKRename.setEnabled(false);
		mnuFKDelete.setEnabled(false);
		mnuEditColumn.setEnabled(false);

		if (table == null) {
			txtTableName.setText("");
			txtSchemaName.setText("");
			txtCatalogName.setText("");
			txtPrimaryKeyName.setText("");

			return;
		}

		mnuEditIndex.setEnabled(true);
		mnuAddUniqueIndex.setEnabled(true);
		mnuAddIndex.setEnabled(true);
		mnuDeleteIndex.setEnabled(true);
		mnuFKRename.setEnabled(true);
		mnuFKDelete.setEnabled(true);
		mnuEditColumn.setEnabled(true);

		txtTableName.setText(table.getName());
		txtTableName.setEnabled(true);

		txtSchemaName.setText(table.getSchemaName() == null ? "" : table.getSchemaName());
		txtSchemaName.setEnabled(true);

		txtCatalogName.setText(table.getCatalogName() == null ? "" : table.getCatalogName());
		txtCatalogName.setEnabled(true);

		if (table.getPrimaryKey() != null) {
			txtPrimaryKeyName.setEnabled(true);
			txtPrimaryKeyName.setText(table.getPrimaryKey().getName());
		}

		table.getColumns().forEach(c -> {
			final var ti = new TableItem(tblColumns, SWT.NONE);
			final var values = new String[4];
			int i = 0;

			values[i++] = c.getName();
			values[i++] = c.getColumnType().getName();

			if (c.isNullable())
				ti.setImage(i, CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_CHECK));
			else
				ti.setImage(i, CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_UNCHECK));

			values[i++] = "";

			if (table.getPrimaryKey() != null && table.getPrimaryKey().getColumn().getName().equals(c.getName()))
				ti.setImage(i, CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_CHECK));
			else
				ti.setImage(i, CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_UNCHECK));

			values[i] = "";

			ti.setText(values);
			ti.setData(c.getName(), c);
		});

		table.getIndexes().forEach(index -> {
			final var ti = new TableItem(tblIndexes, SWT.NONE);
			final var s = new String[3];
			s[0] = index.getName();

			if (index.isUnique())
				ti.setImage(1, CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_CHECK));
			else
				ti.setImage(1, CodeCadenzaResourcePlugin.getImage(IMG_CLIENT_UNCHECK));

			s[1] = "";
			s[2] = "";

			index.getColumns().forEach(c -> s[2] = s[2] + c.getName() + "; ");

			s[2] = s[2].substring(0, s[2].length() - 2);

			ti.setText(s);
			ti.setData(index.getName(), index);
		});

		table.getForeignKeys().forEach(fk -> {
			final var ti = new TableItem(tblForeignKeys, SWT.NONE);
			final var s = new String[5];
			int i = 0;

			s[i++] = fk.getName();
			s[i++] = fk.getColumn().getName();
			s[i++] = fk.getColumn().getColumnType().getName();
			s[i++] = fk.getReferencedColumn().getDatabaseTable().getName();
			s[i++] = fk.getReferencedColumn().getName();

			ti.setText(s);
			ti.setData(fk.getName(), fk);
		});

		txtTableName.addModifyListener(textChangeListener);
		txtSchemaName.addModifyListener(textChangeListener);
		txtCatalogName.addModifyListener(textChangeListener);
		txtPrimaryKeyName.addModifyListener(textChangeListener);
	}

	/**
	 * Validate the input and update the meta-model
	 */
	private void updateMetaModel() {
		final String newPrimaryKeyName = txtPrimaryKeyName.getText();
		final String newTableName = txtTableName.getText();
		final String newSchemaName = txtSchemaName.getText();
		final String newCatalogName = txtCatalogName.getText();
		final Shell shell = Display.getCurrent().getActiveShell();

		try {
			final Database db = table.getDatabase();
			final var dbService = new DBSynchService(db);

			if (!newTableName.equals(table.getName()))
				dbService.validateIdentifier(newTableName);

			if (table.getSchemaName() == null || table.getSchemaName().isEmpty()) {
				if (!newSchemaName.isEmpty())
					dbService.validateIdentifier(newSchemaName);
			}
			else if (!newSchemaName.isEmpty() && !table.getSchemaName().equals(newSchemaName))
				dbService.validateIdentifier(newSchemaName);

			if (table.getCatalogName() == null || table.getCatalogName().isEmpty()) {
				if (!newCatalogName.isEmpty())
					dbService.validateIdentifier(newCatalogName);
			}
			else if (!newCatalogName.isEmpty() && !table.getCatalogName().equals(newCatalogName))
				dbService.validateIdentifier(newCatalogName);

			if (table.getPrimaryKey() != null && !newPrimaryKeyName.equals(table.getPrimaryKey().getName()))
				dbService.validateIdentifier(newPrimaryKeyName);
		}
		catch (final DBObjectValidationException ex) {
			MessageDialog.openInformation(shell, DLG_TITLE, ex.getMessage());
			return;
		}

		editingDomain.getCommandStack().execute(new RecordingCommand(editingDomain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				table.setName(newTableName);
				table.setSchemaName(newSchemaName);
				table.setCatalogName(newCatalogName);

				if (table.getPrimaryKey() != null)
					table.getPrimaryKey().setName(newPrimaryKeyName);
			}
		});
	}

	/**
	 * Edit the database column
	 */
	private void editColumn() {
		if (tblColumns.getSelection() == null || tblColumns.getSelection().length == 0)
			return;

		editingDomain.getCommandStack().execute(new RecordingCommand(editingDomain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				final String columnName = tblColumns.getSelection()[0].getText(0);
				final var column = (DBColumn) tblColumns.getSelection()[0].getData(columnName);
				final Shell shell = DbTablePropertySection.this.getPart().getSite().getShell();
				final var dlg = new EditDBColumnDialog(shell, column);

				if (dlg.open() == Window.OK)
					refresh();
			}
		});
	}

	/**
	 * Add an index to the table
	 * @param unique
	 */
	private void addIndex(final boolean unique) {
		editingDomain.getCommandStack().execute(new RecordingCommand(editingDomain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				if (table.getColumns().isEmpty())
					return;

				final Shell shell = DbTablePropertySection.this.getPart().getSite().getShell();
				final var dlg = new EditIndexDialog(shell, unique, table);

				if (dlg.open() != Window.OK)
					return;

				// Rebuild the domain object source files if the index is unique!
				if (unique) {
					final Project project = table.getDatabase().getProject();

					CodeCadenzaDiagramEditorUtil.rebuildDomainObjects(project, false, true);
				}

				refresh();
			}
		});
	}

	/**
	 * Edit the selected index
	 */
	private void editIndex() {
		if (tblIndexes.getSelection() == null || tblIndexes.getSelection().length == 0)
			return;

		editingDomain.getCommandStack().execute(new RecordingCommand(editingDomain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				final String indexName = tblIndexes.getSelection()[0].getText(0);
				final var index = (DBIndex) tblIndexes.getSelection()[0].getData(indexName);
				final Shell shell = DbTablePropertySection.this.getPart().getSite().getShell();
				final var dlg = new EditIndexDialog(shell, index.isUnique(), index);

				if (dlg.open() != Window.OK)
					return;

				// Rebuild the domain object source files if the index is unique!
				if (index.isUnique()) {
					final Project project = table.getDatabase().getProject();

					CodeCadenzaDiagramEditorUtil.rebuildDomainObjects(project, false, true);
				}

				refresh();
			}
		});
	}

	/**
	 * Delete the selected index
	 */
	private void deleteIndex() {
		if (tblIndexes.getSelection() == null || tblIndexes.getSelection().length == 0)
			return;

		editingDomain.getCommandStack().execute(new RecordingCommand(editingDomain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				final var index = (DBIndex) tblIndexes.getSelection()[0].getData(tblIndexes.getSelection()[0].getText(0));

				table.getIndexes().remove(index);

				// Rebuild the domain object source files if the index is unique!
				if (index.isUnique()) {
					final Project project = table.getDatabase().getProject();

					CodeCadenzaDiagramEditorUtil.rebuildDomainObjects(project, false, true);
				}

				refresh();
			}
		});
	}

	/**
	 * Rename the selected foreign key
	 */
	private void renameForeignKey() {
		if (tblForeignKeys.getSelection() == null || tblForeignKeys.getSelection().length == 0)
			return;

		final String fkName = tblForeignKeys.getSelection()[0].getText(0);
		final var foreignKey = (ForeignKey) tblForeignKeys.getSelection()[0].getData(fkName);
		final Shell shell = DbTablePropertySection.this.getPart().getSite().getShell();

		final var dlg = new InputDialog(shell, "Rename foreign key", "Enter a new foreign key name:", fkName, newText -> {
			try {
				final Database db = foreignKey.getTable().getDatabase();

				new DBSynchService(db).validateIdentifier(newText);
			}
			catch (final DBObjectValidationException ex) {
				return ex.getMessage();
			}

			return null;
		});

		if (dlg.open() != Window.OK)
			return;

		editingDomain.getCommandStack().execute(new RecordingCommand(editingDomain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				foreignKey.setName(dlg.getValue());
			}
		});

		refresh();
	}

	/**
	 * Delete the selected foreign key
	 */
	private void deleteForeignKey() {
		if (tblForeignKeys.getSelection() == null || tblForeignKeys.getSelection().length == 0)
			return;

		editingDomain.getCommandStack().execute(new RecordingCommand(editingDomain) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				final String fkName = tblForeignKeys.getSelection()[0].getText(0);
				final var foreignKey = (ForeignKey) tblForeignKeys.getSelection()[0].getData(fkName);

				table.getForeignKeys().remove(foreignKey);

				refresh();
			}
		});
	}

}
