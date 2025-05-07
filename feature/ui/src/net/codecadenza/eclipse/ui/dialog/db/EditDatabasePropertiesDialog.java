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
package net.codecadenza.eclipse.ui.dialog.db;

import static net.codecadenza.eclipse.shared.Constants.REMOVE_DEFAULT_MENU_ITEMS;

import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.IdentifierStyleEnumeration;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.runtime.richclient.eclipse.widget.ColumnSortType;
import net.codecadenza.runtime.richclient.eclipse.widget.DataGridComposite;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for maintaining database properties
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDatabasePropertiesDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit database properties";

	private Text txtVendorGroup;
	private Text txtHibernateDialect;
	private Text txtMaxIdentifierLength;
	private Text txtEclipseLinkTargetDBName;
	private Text txtIdentifierRegEx;
	private Button chkIdentityColumnSupport;
	private Button chkSequenceSupport;
	private Combo cboIdentifierStyle;
	private Text txtReservedWords;
	private DataGridComposite<DBColumnType> gridColumnTypes;
	private final Database database;
	private Text txtSchemaName;
	private Text txtCatalogName;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param database
	 */
	public EditDatabasePropertiesDialog(Shell parentShell, Database database) {
		super(parentShell);

		this.database = database;
	}

	/**
	 * Refresh the grid that contains the database column types
	 */
	private void refreshDBColumnTypeGrid() {
		final var columnTypes = new BasicEList<DBColumnType>();
		columnTypes.addAll(database.getAllSupportedColumnTypes());
		columnTypes.sort((columnType1, columnType2) -> columnType1.getName().compareTo(columnType2.getName()));

		gridColumnTypes.setData(columnTypes);
	}

	/**
	 * Initialize form data
	 */
	private void initFormData() {
		txtVendorGroup.setText(database.getVendorGroup().getName());
		txtMaxIdentifierLength.setText(Integer.toString(database.getMaxIdentifierLength()));
		txtIdentifierRegEx.setText(database.getIdentifierRegEx());
		chkIdentityColumnSupport.setSelection(database.isSupportsIdentityColumn());
		chkSequenceSupport.setSelection(database.isSupportsSequence());
		txtEclipseLinkTargetDBName.setText(database.getEclipseLinkTargetDBName());
		txtHibernateDialect.setText(database.getHibernateDialect());
		txtReservedWords.setText(database.getReservedWords());
		txtSchemaName.setText(database.getSchemaName() == null ? "" : database.getSchemaName());
		txtCatalogName.setText(database.getCatalogName() == null ? "" : database.getCatalogName());

		for (final IdentifierStyleEnumeration style : IdentifierStyleEnumeration.values())
			cboIdentifierStyle.add(style.name());

		for (int i = 0; i < cboIdentifierStyle.getItems().length; i++) {
			final String item = cboIdentifierStyle.getItem(i);

			if (item.equals(database.getIdentifierStyle().name())) {
				cboIdentifierStyle.select(i);
				break;
			}
		}

		refreshDBColumnTypeGrid();
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean validateFormData() {
		String inputToCheck = txtSchemaName.getText();

		if (!inputToCheck.isEmpty()) {
			try {
				new DBSynchService(database).validateIdentifier(txtSchemaName.getText());
			}
			catch (final DBObjectValidationException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, e.getMessage());
				return false;
			}
		}

		inputToCheck = txtCatalogName.getText();

		if (!inputToCheck.isEmpty()) {
			try {
				new DBSynchService(database).validateIdentifier(txtCatalogName.getText());
			}
			catch (final DBObjectValidationException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, e.getMessage());
				return false;
			}
		}

		inputToCheck = txtHibernateDialect.getText();

		if (inputToCheck.isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The Hibernate dialect must not be empty!");
			txtHibernateDialect.setFocus();
			return false;
		}

		inputToCheck = txtMaxIdentifierLength.getText();

		if (inputToCheck.isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The max. identifier length must not be empty!");
			txtMaxIdentifierLength.setFocus();
			return false;
		}

		try {
			if (Integer.parseInt(inputToCheck) <= 0) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The max. identifier length must be greater than 0!");
				txtMaxIdentifierLength.setFocus();
				return false;
			}
		}
		catch (final NumberFormatException p) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The max. identifier length requires an integer value!");
			txtMaxIdentifierLength.setFocus();
			return false;
		}

		inputToCheck = txtEclipseLinkTargetDBName.getText();

		if (inputToCheck.isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The EclipseLink target DB name must not be empty!");
			txtEclipseLinkTargetDBName.setFocus();
			return false;
		}

		inputToCheck = txtIdentifierRegEx.getText();

		if (inputToCheck.isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The regular expression for database identifiers must not be empty!");
			txtIdentifierRegEx.setFocus();
			return false;
		}

		try {
			Pattern.compile(inputToCheck);
		}
		catch (final PatternSyntaxException e) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The regular expression for database identifiers is not valid!");
			txtIdentifierRegEx.setFocus();
			return false;
		}

		return true;
	}

	/**
	 * Save form data
	 * @return true if the data was saved successfully
	 */
	private boolean saveData() {
		boolean rebuildORMXML = false;
		final var currentSchemaName = database.getSchemaName() == null ? "" : database.getSchemaName();
		final var currentCatalogName = database.getCatalogName() == null ? "" : database.getCatalogName();

		if (!currentSchemaName.equals(txtSchemaName.getText()) || !currentCatalogName.equals(txtCatalogName.getText()))
			rebuildORMXML = true;

		database.setHibernateDialect(txtHibernateDialect.getText());
		database.setMaxIdentifierLength(Integer.parseInt(txtMaxIdentifierLength.getText()));
		database.setEclipseLinkTargetDBName(txtEclipseLinkTargetDBName.getText());
		database.setIdentifierRegEx(txtIdentifierRegEx.getText());
		database.setSupportsIdentityColumn(chkIdentityColumnSupport.getSelection());
		database.setSupportsSequence(chkSequenceSupport.getSelection());
		database.setReservedWords(txtReservedWords.getText());
		database.setSchemaName(txtSchemaName.getText());
		database.setCatalogName(txtCatalogName.getText());

		for (final IdentifierStyleEnumeration item : IdentifierStyleEnumeration.values()) {
			final String selectedItem = cboIdentifierStyle.getItem(cboIdentifierStyle.getSelectionIndex());

			if (item.name().equals(selectedItem)) {
				database.setIdentifierStyle(item);
				break;
			}
		}

		try {
			EclipseIDEService.saveProjectMetaData(database.getProject());

			if (rebuildORMXML)
				ProjectBuildFactory.getBuildService(database.getProject()).rebuildORMXML();

			return true;
		}
		catch (final Exception e) {
			CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(e);
		}

		return false;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var tabFolder = new TabFolder(panDialogArea, SWT.NONE);
		tabFolder.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var tabBasicData = new TabItem(tabFolder, SWT.NONE);
		tabBasicData.setText("Basic data");

		final var panBasicData = new Composite(tabFolder, SWT.NONE);
		panBasicData.setLayout(new GridLayout(4, false));

		tabBasicData.setControl(panBasicData);

		final var lblVendorGroup = new Label(panBasicData, SWT.NONE);
		lblVendorGroup.setText("Database vendor group:");

		final var gdVendorGroup = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdVendorGroup.widthHint = 150;

		txtVendorGroup = new Text(panBasicData, SWT.BORDER);
		txtVendorGroup.setEnabled(false);
		txtVendorGroup.setLayoutData(gdVendorGroup);

		final var lblIdentifierStyle = new Label(panBasicData, SWT.NONE);
		lblIdentifierStyle.setText("Identifier style in target DB:");

		cboIdentifierStyle = new Combo(panBasicData, SWT.READ_ONLY);
		cboIdentifierStyle.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblSchema = new Label(panBasicData, SWT.NONE);
		lblSchema.setText("Schema name:");

		txtSchemaName = new Text(panBasicData, SWT.BORDER);
		txtSchemaName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblCatalog = new Label(panBasicData, SWT.NONE);
		lblCatalog.setText("Catalog name:");

		txtCatalogName = new Text(panBasicData, SWT.BORDER);
		txtCatalogName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblHibernateDialect = new Label(panBasicData, SWT.NONE);
		lblHibernateDialect.setText("Hibernate dialect:");

		txtHibernateDialect = new Text(panBasicData, SWT.BORDER);
		txtHibernateDialect.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));

		final var lblMaxIdentifierLength = new Label(panBasicData, SWT.NONE);
		lblMaxIdentifierLength.setText("Max. identifier length:");

		txtMaxIdentifierLength = new Text(panBasicData, SWT.BORDER);
		txtMaxIdentifierLength.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblEclipseLinkTargetDBName = new Label(panBasicData, SWT.NONE);
		lblEclipseLinkTargetDBName.setText("EclipseLink target DB name:");

		txtEclipseLinkTargetDBName = new Text(panBasicData, SWT.BORDER);
		txtEclipseLinkTargetDBName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblIdentifierRegEx = new Label(panBasicData, SWT.NONE);
		lblIdentifierRegEx.setText("Identifier regular expression:");

		txtIdentifierRegEx = new Text(panBasicData, SWT.BORDER);
		txtIdentifierRegEx.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));

		final var lblIdentityColumnSupport = new Label(panBasicData, SWT.NONE);
		lblIdentityColumnSupport.setText("Supports identity columns:");

		chkIdentityColumnSupport = new Button(panBasicData, SWT.CHECK);

		final var lblSequenceSupport = new Label(panBasicData, SWT.NONE);
		lblSequenceSupport.setText("Supports sequences:");

		chkSequenceSupport = new Button(panBasicData, SWT.CHECK);

		final var tabReservedWords = new TabItem(tabFolder, SWT.NONE);
		tabReservedWords.setText("Reserved words");

		final var panReservedWords = new Composite(tabFolder, SWT.NONE);
		panReservedWords.setLayout(new GridLayout());

		tabReservedWords.setControl(panReservedWords);

		final var gdReservedWords = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdReservedWords.heightHint = 100;
		gdReservedWords.widthHint = 500;

		txtReservedWords = new Text(panReservedWords, SWT.BORDER | SWT.WRAP | SWT.H_SCROLL | SWT.V_SCROLL | SWT.CANCEL | SWT.MULTI);
		txtReservedWords.setLayoutData(gdReservedWords);

		final var glColumnTypes = new GridLayout();
		glColumnTypes.marginHeight = 0;

		final var panColumnTypes = new Composite(panDialogArea, SWT.NONE);
		panColumnTypes.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		panColumnTypes.setLayout(glColumnTypes);

		final var lblDatabaseColumnTypes = new Label(panColumnTypes, SWT.NONE);
		lblDatabaseColumnTypes.setText("Database column types:");

		gridColumnTypes = new DataGridComposite<>(panColumnTypes, SWT.BORDER, REMOVE_DEFAULT_MENU_ITEMS) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#getCellText(java.lang.Object, int)
			 */
			@Override
			public String getCellText(DBColumnType element, int columnIndex) {
				if (columnIndex == 0)
					return element.getName();

				return Boolean.toString(element.isOmitSizeInformation());
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.__AbstractDataGridComposite#onDoubleClick(java.lang.Object)
			 */
			@Override
			public void onDoubleClick(DBColumnType selDBColumnType) {
				if (selDBColumnType == null)
					return;

				final var dlg = new EditDBColumnTypeDialog(parentShell, selDBColumnType, database);

				if (dlg.open() == OK)
					refreshDBColumnTypeGrid();
			}
		};

		final var gdColumnType = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdColumnType.heightHint = 400;

		gridColumnTypes.setLayoutData(gdColumnType);
		gridColumnTypes.addColumn("Name", ColumnSortType.STRING, 250);
		gridColumnTypes.addColumn("Omit size information", ColumnSortType.STRING, 200);

		final var itemCreateNew = new MenuItem(gridColumnTypes.getPopUpMenu(), SWT.NONE);
		itemCreateNew.setText("Create new");

		itemCreateNew.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final var dlg = new EditDBColumnTypeDialog(getShell(), database);

				if (dlg.open() == OK)
					refreshDBColumnTypeGrid();
			}
		});

		final var itemEdit = new MenuItem(gridColumnTypes.getPopUpMenu(), SWT.NONE);
		itemEdit.setText("Edit");

		itemEdit.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final DBColumnType selDBColumnType = gridColumnTypes.getSelection();

				if (selDBColumnType == null)
					return;

				final var dlg = new EditDBColumnTypeDialog(getShell(), selDBColumnType, database);

				if (dlg.open() == OK)
					refreshDBColumnTypeGrid();
			}
		});

		initFormData();

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (!validateFormData())
				return;

			if (!saveData())
				return;
		}

		super.buttonPressed(buttonId);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText(DLG_TITLE);
	}

}
