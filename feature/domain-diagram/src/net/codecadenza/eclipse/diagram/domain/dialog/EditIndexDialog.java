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
package net.codecadenza.eclipse.diagram.domain.dialog;

import static net.codecadenza.eclipse.shared.Constants.IMG_INDEX_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_INDEX_EDIT;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY_ADD;
import static net.codecadenza.eclipse.shared.Constants.IMG_KEY_EDIT;

import java.util.Collection;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.CheckboxTableViewer;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining database indexes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditIndexDialog extends CodeCadenzaDialog {
	private Text txtName;
	private final EList<DBColumn> selColumns = new BasicEList<>();
	private final EList<DBColumn> allColumns = new BasicEList<>();
	private final boolean unique;
	private final DBTable table;
	private boolean doEdit;
	private DBIndex index;
	private CheckboxTableViewer checkboxTableViewer;
	private String title;

	/**
	 * Create the dialog for creating new indexes
	 * @param parentShell
	 * @param unique
	 * @param table
	 */
	public EditIndexDialog(Shell parentShell, boolean unique, DBTable table) {
		super(parentShell);

		this.table = table;
		this.unique = unique;

		if (unique)
			this.title = "Add new unique key to table " + table.getConvertedName();
		else
			this.title = "Add index to table " + table.getConvertedName();
	}

	/**
	 * Create the dialog for maintaining existing indexes
	 * @param parentShell
	 * @param unique
	 * @param index
	 */
	public EditIndexDialog(Shell parentShell, boolean unique, DBIndex index) {
		super(parentShell);

		this.table = index.getTable();
		this.unique = unique;
		this.doEdit = true;
		this.index = index;

		if (unique)
			this.title = "Edit unique key " + index.getConvertedName();
		else
			this.title = "Edit index " + index.getConvertedName();
	}

	/**
	 * Content provider for the viewer
	 */
	class ContentProvider implements IStructuredContentProvider {
		/*
		 * (non-Javadoc)
		 * @see org.eclipse.jface.viewers.IStructuredContentProvider#getElements(java.lang.Object)
		 */
		@Override
		@SuppressWarnings("unchecked")
		public Object[] getElements(Object inputElement) {
			return ((Collection<DBColumn>) inputElement).toArray();
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
			final var column = (DBColumn) element;

			switch (columnIndex) {
				case 1:
					return column.getName();
				case 2:
					return column.getColumnType().getName();
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

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		// Check if the dialog works in edit mode
		if (doEdit) {
			for (final DBColumn tableCol : table.getColumns()) {
				allColumns.add(tableCol);

				for (final DBColumn col : index.getColumns())
					if (tableCol.equals(col)) {
						selColumns.add(tableCol);
						break;
					}
			}
		}
		else
			table.getColumns().forEach(allColumns::add);

		final var panDialogArea = (Composite) super.createDialogArea(parent, 2);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (doEdit)
			txtName.setText(index.getName());

		final var gdGroupColumns = new GridData(SWT.FILL, SWT.FILL, true, true, 2, 1);
		gdGroupColumns.heightHint = 400;
		gdGroupColumns.widthHint = 500;
		gdGroupColumns.verticalIndent = 5;

		final var groupColumns = new Group(panDialogArea, SWT.NONE);
		groupColumns.setText("Select columns to be added to index");
		groupColumns.setLayoutData(gdGroupColumns);
		groupColumns.setLayout(new GridLayout());

		checkboxTableViewer = CheckboxTableViewer.newCheckList(groupColumns, SWT.BORDER);
		checkboxTableViewer.setContentProvider(new ContentProvider());
		checkboxTableViewer.setLabelProvider(new TableLabelProvider());

		final Table tabControl = checkboxTableViewer.getTable();
		tabControl.setHeaderVisible(true);
		tabControl.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var colSel = new TableColumn(tabControl, SWT.NONE);
		colSel.setWidth(40);

		final var colColumnName = new TableColumn(tabControl, SWT.NONE);
		colColumnName.setWidth(250);
		colColumnName.setText("Column name");

		final var colColumnType = new TableColumn(tabControl, SWT.NONE);
		colColumnType.setWidth(150);
		colColumnType.setText("Column type");

		checkboxTableViewer.setInput(allColumns);
		checkboxTableViewer.setCheckedElements(selColumns.toArray());

		getShell().setText(title);

		if (unique) {
			if (!doEdit)
				getShell().setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_ADD));
			else
				getShell().setImage(CodeCadenzaResourcePlugin.getImage(IMG_KEY_EDIT));
		}
		else if (!doEdit)
			getShell().setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX_ADD));
		else
			getShell().setImage(CodeCadenzaResourcePlugin.getImage(IMG_INDEX_EDIT));

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			// Validate the user input
			try {
				final Database db = table.getDatabase();

				new DBSynchService(db).validateIdentifier(txtName.getText());
			}
			catch (final DBObjectValidationException ex) {
				MessageDialog.openInformation(getShell(), title, ex.getMessage());

				txtName.setFocus();
				return;
			}

			if (checkboxTableViewer.getCheckedElements().length == 0) {
				MessageDialog.openInformation(getShell(), title, "One column must be selected at least!");
				return;
			}

			if (!doEdit) {
				// Create and save the index
				index = DbFactory.eINSTANCE.createDBIndex();
				index.setName(txtName.getText());

				// Add the columns to the index
				for (final Object obj : checkboxTableViewer.getCheckedElements()) {
					final var col = (DBColumn) obj;
					index.getColumns().add(col);
				}

				index.setUnique(unique);
				index.setTable(table);

				table.getIndexes().add(index);
			}
			else {
				// Save the index
				index.setName(txtName.getText());
				index.getColumns().clear();

				// Add the columns to the index
				for (final Object obj : checkboxTableViewer.getCheckedElements()) {
					final var col = (DBColumn) obj;
					index.getColumns().add(col);
				}
			}
		}

		super.buttonPressed(buttonId);
	}

}
