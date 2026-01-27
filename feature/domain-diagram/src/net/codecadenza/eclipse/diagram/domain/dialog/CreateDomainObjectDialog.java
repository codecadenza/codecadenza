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

import static net.codecadenza.eclipse.shared.Constants.DB_TABLE_SUFFIX;

import java.util.ArrayList;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.domain.DiscriminatorColumnTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.IDGenerator;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.InheritanceTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaTitleAreaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.FocusAdapter;
import org.eclipse.swt.events.FocusEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating new domain objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateDomainObjectDialog extends CodeCadenzaTitleAreaDialog {
	private static final String DEFAULT_DOMAIN_OBJ_NAME = "DomainObject";

	private final DomainObject domainObject;
	private final Namespace namespace;
	private final Project project;
	private Text txtComment;
	private Text txtLabelPlural;
	private Text txtNamePlural;
	private Text txtBlockSize;
	private Text txtInitialValue;
	private Text txtSchemaName;
	private Text txtCatalogName;
	private Combo cboIdGeneratorType;
	private Button chkPropertyAccess;
	private Button chkAbstract;
	private Button chkMappedSuperClass;
	private Text txtLabel;
	private Text txtDomainObjectName;
	private Text txtColumnName;
	private Text txtDiscriminatorValue;
	private Combo cboInheritance;
	private Combo cboDiscriminatorType;
	private Combo cboTag;
	private Text txtTableName;
	private boolean propertyAccess;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param domainObject
	 * @param namespace
	 */
	public CreateDomainObjectDialog(Shell parentShell, DomainObject domainObject, Namespace namespace) {
		super(parentShell);

		this.domainObject = domainObject;
		this.namespace = namespace;
		this.project = namespace.getProject();

		// Set the default value for the JPA annotation strategy (property or field)
		for (final Namespace n : project.getDomainNamespace().getChildNamespaces())
			for (final JavaType j : n.getJavaTypes())
				if (j instanceof final DomainObject d) {
					propertyAccess = d.isPropertyAccess();
					break;
				}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));
		groupBasicData.setLayout(new GridLayout(6, false));

		final var lblName = new Label(groupBasicData, SWT.LEFT);
		lblName.setText("Domain object name:");

		txtDomainObjectName = new Text(groupBasicData, SWT.SINGLE | SWT.BORDER);
		txtDomainObjectName.setText(DEFAULT_DOMAIN_OBJ_NAME);
		txtDomainObjectName.selectAll();
		txtDomainObjectName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 5, 1));
		txtDomainObjectName.setFocus();

		txtDomainObjectName.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				txtNamePlural.setText(EclipseIDEService.buildDefaultPluralForm(txtDomainObjectName.getText()));
				txtLabel.setText(EclipseIDEService.buildDefaultLabel(txtDomainObjectName.getText()));
				txtLabelPlural.setText(EclipseIDEService.buildDefaultPluralLabel(txtDomainObjectName.getText()));
				txtComment.setText(EclipseIDEService.buildDomainObjectComment(txtLabel.getText()));
				txtTableName.setText(txtLabel.getText().toLowerCase().replace(" ", "_") + DB_TABLE_SUFFIX);
			}
		});

		final var lblNamePlural = new Label(groupBasicData, SWT.NONE);
		lblNamePlural.setText("Plural name:");

		txtNamePlural = new Text(groupBasicData, SWT.SINGLE | SWT.BORDER);
		txtNamePlural.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 5, 1));

		final var lblLabel = new Label(groupBasicData, SWT.NONE);
		lblLabel.setText("Label:");

		txtLabel = new Text(groupBasicData, SWT.BORDER);
		txtLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 5, 1));

		txtLabel.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				txtLabelPlural.setText(EclipseIDEService.buildDefaultPluralForm(txtLabel.getText()));
				txtComment.setText(EclipseIDEService.buildDomainObjectComment(txtLabel.getText()));
			}
		});

		final var lblLabelPlural = new Label(groupBasicData, SWT.NONE);
		lblLabelPlural.setText("Plural label:");

		txtLabelPlural = new Text(groupBasicData, SWT.BORDER);
		txtLabelPlural.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 5, 1));

		final var lblPropertyAccess = new Label(groupBasicData, SWT.NONE);
		lblPropertyAccess.setText("Property access:");

		chkPropertyAccess = new Button(groupBasicData, SWT.CHECK);
		chkPropertyAccess.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		chkPropertyAccess.setSelection(propertyAccess);

		final var lblAbstract = new Label(groupBasicData, SWT.NONE);
		lblAbstract.setText("Abstract class:");

		chkAbstract = new Button(groupBasicData, SWT.CHECK);
		chkAbstract.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));
		chkAbstract.setSelection(true);

		chkAbstract.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean enableInheritanceFields = false;

				if (chkAbstract.getSelection())
					chkMappedSuperClass.setSelection(false);

				cboInheritance.setEnabled(true);

				final InheritanceTypeEnumeration selection = InheritanceTypeEnumeration
						.valueOf(cboInheritance.getItem(cboInheritance.getSelectionIndex()));

				if (selection != InheritanceTypeEnumeration.NONE)
					enableInheritanceFields = true;

				txtDiscriminatorValue.setEnabled(enableInheritanceFields);
				cboDiscriminatorType.setEnabled(enableInheritanceFields);
				txtColumnName.setEnabled(enableInheritanceFields);

				setTitleAreaMessage();

				updateTagList();
			}
		});

		chkAbstract.setSelection(false);

		final var lblMappedSuperClass = new Label(groupBasicData, SWT.NONE);
		lblMappedSuperClass.setText("Mapped-superclass:");

		chkMappedSuperClass = new Button(groupBasicData, SWT.CHECK);
		chkMappedSuperClass.setLayoutData(new GridData(SWT.LEFT, SWT.CENTER, true, false));
		chkMappedSuperClass.setSelection(true);

		chkMappedSuperClass.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				boolean enableInheritanceFields = false;

				if (chkMappedSuperClass.getSelection())
					chkAbstract.setSelection(false);

				final InheritanceTypeEnumeration selection = InheritanceTypeEnumeration
						.valueOf(cboInheritance.getItem(cboInheritance.getSelectionIndex()));

				if (selection != InheritanceTypeEnumeration.NONE && !chkMappedSuperClass.getSelection())
					enableInheritanceFields = true;

				cboInheritance.setEnabled(!chkMappedSuperClass.getSelection());

				txtDiscriminatorValue.setEnabled(enableInheritanceFields);
				cboDiscriminatorType.setEnabled(enableInheritanceFields);
				txtColumnName.setEnabled(enableInheritanceFields);

				setTitleAreaMessage();

				updateTagList();
			}
		});

		chkMappedSuperClass.setSelection(false);

		final var lblTag = new Label(groupBasicData, SWT.NONE);
		lblTag.setText("Tag:");

		cboTag = new Combo(groupBasicData, SWT.READ_ONLY);
		cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 5, 1));

		final var lblComment = new Label(groupBasicData, SWT.NONE);
		lblComment.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
		lblComment.setText("Comment:");

		final var gdComment = new GridData(SWT.FILL, SWT.FILL, true, true, 5, 1);
		gdComment.heightHint = 100;

		txtComment = new Text(groupBasicData, SWT.MULTI | SWT.BORDER);
		txtComment.setLayoutData(gdComment);

		updateTagList();

		final var tabFolderAddData = new TabFolder(panDialogArea, SWT.NONE);
		tabFolderAddData.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));

		final var tabItemDBMapping = new TabItem(tabFolderAddData, SWT.NONE);
		tabItemDBMapping.setText("Database mapping");

		final var panDBMapping = new Composite(tabFolderAddData, SWT.NONE);
		panDBMapping.setLayout(new GridLayout(2, false));

		tabItemDBMapping.setControl(panDBMapping);

		final var lblTableName = new Label(panDBMapping, SWT.LEFT);
		lblTableName.setText("Table name:");

		txtTableName = new Text(panDBMapping, SWT.SINGLE | SWT.BORDER);
		txtTableName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblSchemaName = new Label(panDBMapping, SWT.LEFT);
		lblSchemaName.setText("Schema name:");

		txtSchemaName = new Text(panDBMapping, SWT.SINGLE | SWT.BORDER);
		txtSchemaName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblCatalogName = new Label(panDBMapping, SWT.LEFT);
		lblCatalogName.setText("Catalog name:");

		txtCatalogName = new Text(panDBMapping, SWT.SINGLE | SWT.BORDER);
		txtCatalogName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var tabItemIDGenerator = new TabItem(tabFolderAddData, SWT.NONE);
		tabItemIDGenerator.setText("ID generator");

		final var panIDGenerator = new Composite(tabFolderAddData, SWT.NONE);
		panIDGenerator.setLayout(new GridLayout(2, false));

		tabItemIDGenerator.setControl(panIDGenerator);

		final var lblIdgeneratorType = new Label(panIDGenerator, SWT.NONE);
		lblIdgeneratorType.setText("ID generator type:");

		cboIdGeneratorType = new Combo(panIDGenerator, SWT.READ_ONLY);
		cboIdGeneratorType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final IDGeneratorTypeEnumeration type : IDGeneratorTypeEnumeration.values()) {
			if (type == IDGeneratorTypeEnumeration.SEQUENCE && !project.getDatabase().isSupportsSequence())
				continue;

			if (type == IDGeneratorTypeEnumeration.IDENTITY && !project.getDatabase().isSupportsIdentityColumn())
				continue;

			cboIdGeneratorType.add(type.name());
		}

		if (project.getDatabase().isSupportsSequence()) {
			for (int i = 0; i < cboIdGeneratorType.getItemCount(); i++)
				if (cboIdGeneratorType.getItem(i).equals(IDGeneratorTypeEnumeration.SEQUENCE.name())) {
					cboIdGeneratorType.select(i);
					break;
				}
		}
		else
			for (int i = 0; i < cboIdGeneratorType.getItemCount(); i++)
				if (cboIdGeneratorType.getItem(i).equals(IDGeneratorTypeEnumeration.IDENTITY.name())) {
					cboIdGeneratorType.select(i);
					break;
				}

		if (cboIdGeneratorType.getSelectionIndex() == -1)
			cboIdGeneratorType.select(0);

		cboIdGeneratorType.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final IDGeneratorTypeEnumeration type = IDGeneratorTypeEnumeration
						.valueOf(cboIdGeneratorType.getItem(cboIdGeneratorType.getSelectionIndex()));

				if (type == IDGeneratorTypeEnumeration.NONE || type == IDGeneratorTypeEnumeration.IDENTITY
						|| type == IDGeneratorTypeEnumeration.UUID) {
					txtInitialValue.setText("");
					txtBlockSize.setText("");
					txtInitialValue.setEnabled(false);
					txtBlockSize.setEnabled(false);
				}
				else {
					txtInitialValue.setText("1");
					txtBlockSize.setText("20");
					txtInitialValue.setEnabled(true);
					txtBlockSize.setEnabled(true);
				}
			}
		});

		final var lblInitialValue = new Label(panIDGenerator, SWT.NONE);
		lblInitialValue.setText("Initial value:");

		txtInitialValue = new Text(panIDGenerator, SWT.BORDER);
		txtInitialValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtInitialValue.setEnabled(false);

		final var lblBlockSize = new Label(panIDGenerator, SWT.NONE);
		lblBlockSize.setText("Block size:");

		txtBlockSize = new Text(panIDGenerator, SWT.BORDER);
		txtBlockSize.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtBlockSize.setEnabled(false);

		if (project.getDatabase().isSupportsSequence()) {
			txtInitialValue.setText("1");
			txtBlockSize.setText("20");
			txtInitialValue.setEnabled(true);
			txtBlockSize.setEnabled(true);
		}

		final var tabItemInheritance = new TabItem(tabFolderAddData, SWT.NONE);
		tabItemInheritance.setText("Inheritance");

		final var panInheritance = new Composite(tabFolderAddData, SWT.NONE);
		panInheritance.setLayout(new GridLayout(2, false));

		tabItemInheritance.setControl(panInheritance);

		final var lblInheritance = new Label(panInheritance, SWT.NONE);
		lblInheritance.setText("Inheritance:");

		cboInheritance = new Combo(panInheritance, SWT.READ_ONLY);
		cboInheritance.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		cboInheritance.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final InheritanceTypeEnumeration selection = InheritanceTypeEnumeration
						.valueOf(cboInheritance.getItem(cboInheritance.getSelectionIndex()));

				if (selection == InheritanceTypeEnumeration.NONE) {
					cboDiscriminatorType.setEnabled(false);
					txtColumnName.setEnabled(false);
					txtDiscriminatorValue.setEnabled(false);
					txtColumnName.setText("");
					txtDiscriminatorValue.setText("");
				}
				else {
					cboDiscriminatorType.setEnabled(true);
					cboDiscriminatorType.select(0);
					txtColumnName.setEnabled(true);
					txtColumnName.setText("discriminator");
					txtDiscriminatorValue.setEnabled(true);
				}
			}
		});

		final var lblDiscriminatorType = new Label(panInheritance, SWT.NONE);
		lblDiscriminatorType.setText("Disc. type:");

		for (final InheritanceTypeEnumeration t : InheritanceTypeEnumeration.values())
			cboInheritance.add(t.toString());

		cboInheritance.select(0);

		cboDiscriminatorType = new Combo(panInheritance, SWT.READ_ONLY);
		cboDiscriminatorType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboDiscriminatorType.select(0);
		cboDiscriminatorType.setEnabled(false);

		final var lblDiscriminatorValue = new Label(panInheritance, SWT.NONE);
		lblDiscriminatorValue.setText("Disc. value:");

		for (final DiscriminatorColumnTypeEnumeration t : DiscriminatorColumnTypeEnumeration.values())
			cboDiscriminatorType.add(t.toString());

		txtDiscriminatorValue = new Text(panInheritance, SWT.BORDER);
		txtDiscriminatorValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtDiscriminatorValue.setEnabled(false);

		final var lblDiscColName = new Label(panInheritance, SWT.NONE);
		lblDiscColName.setText("Disc. column name:");

		txtColumnName = new Text(panInheritance, SWT.BORDER);
		txtColumnName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtColumnName.setEnabled(false);

		// Set the title and the title area message
		setTitleAreaMessage();

		return panDialogArea;
	}

	/**
	 * Set the title and the title area message
	 */
	private void setTitleAreaMessage() {
		// Set the title
		setTitle("Create new domain object");

		if (chkAbstract.getSelection())
			setMessage("Insert data for new abstract class");

		if (chkMappedSuperClass.getSelection())
			setMessage("Insert data for new mapped superclass");

		if (!chkMappedSuperClass.getSelection() && !this.chkAbstract.getSelection())
			setMessage("Insert data for new domain object");
	}

	/**
	 * Update the list of allowed domain object tags
	 */
	private void updateTagList() {
		final ArrayList<String> tagList = project.getValidDomainObjectTags(chkAbstract.getSelection(),
				chkMappedSuperClass.getSelection());
		cboTag.removeAll();

		tagList.forEach(cboTag::add);

		for (int i = 0; i < cboTag.getItemCount(); i++)
			if (cboTag.getItem(i).equals(DomainTagEnumeration.NONE.getName())) {
				cboTag.select(i);
				break;
			}
	}

	/**
	 * Validate all inputs
	 * @return true if the validation was successful
	 */
	protected boolean validateAllInputs() {
		boolean ok = validateDomainObjectName(txtDomainObjectName.getText());

		if (ok)
			ok = validateDomainObjectName(txtNamePlural.getText());

		if (ok)
			ok = validateInitialValueInput(txtInitialValue.getText());

		if (ok)
			ok = validateLabelInput(txtLabel.getText());

		if (ok)
			ok = validateLabelPluralInput(txtLabelPlural.getText());

		if (ok)
			ok = validateBlockSizeInput(txtBlockSize.getText());

		if (ok)
			ok = validateTableNameInput(txtTableName.getText());

		if (ok)
			ok = validateDiscriminatorValueInput(txtDiscriminatorValue.getText());

		if (ok)
			ok = validateDiscriminatorColumnNameInput(txtColumnName.getText());

		if (ok)
			ok = validateDBIdentifier(txtSchemaName.getText());

		if (ok)
			ok = validateDBIdentifier(txtCatalogName.getText());

		// Check if a class with same name already exists!
		for (final Namespace ns : project.getDomainNamespace().getChildNamespaces())
			for (final JavaType type : ns.getJavaTypes())
				if (!type.equals(domainObject) && type.getName().equals(txtDomainObjectName.getText())) {
					setErrorMessage("A domain object or an enumeration with this name already exists!");
					ok = false;
					break;
				}

		return ok;
	}

	/**
	 * Validate the domain object name
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateDomainObjectName(String text) {
		final IStatus status = EclipseIDEService.validateJavaTypeName(text);

		if (status.getSeverity() > IStatus.INFO) {
			setErrorMessage(status.getMessage());
			return false;
		}

		return true;
	}

	/**
	 * Validate the label
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateLabelInput(String text) {
		if (text.isEmpty()) {
			setErrorMessage("The label must not be empty!");
			return false;
		}

		return true;
	}

	/**
	 * Validate the initial value
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateInitialValueInput(String text) {
		if (text.isEmpty() && cboIdGeneratorType.getItem(cboIdGeneratorType.getSelectionIndex())
				.equals(IDGeneratorTypeEnumeration.SEQUENCE.toString())) {
			setErrorMessage("The initial value must not be empty!");
			return false;
		}
		else if (!text.isEmpty()) {
			try {
				Integer.parseInt(text);
			}
			catch (final Exception _) {
				setErrorMessage("The initial value requires an integer value!");
				return false;
			}
		}

		return true;
	}

	/**
	 * Validate the plural label
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateLabelPluralInput(String text) {
		if (text.isEmpty()) {
			setErrorMessage("The plural form of the label must not be empty!");
			return false;
		}

		return true;
	}

	/**
	 * Validate the block size
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateBlockSizeInput(String text) {
		if (text.isEmpty() && cboIdGeneratorType.getItem(cboIdGeneratorType.getSelectionIndex())
				.equals(IDGeneratorTypeEnumeration.SEQUENCE.toString())) {
			setErrorMessage("The block size must not be empty!");
			return false;
		}
		else if (!text.isEmpty()) {
			try {
				Integer.parseInt(text);
			}
			catch (final Exception _) {
				setErrorMessage("The block size requires an integer value!");
				return false;
			}
		}

		return true;
	}

	/**
	 * Validate the table name
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateTableNameInput(String text) {
		final Database db = project.getDatabase();

		try {
			new DBSynchService(db).validateTableName(txtSchemaName.getText(), txtCatalogName.getText(), text);
		}
		catch (final DBObjectValidationException e) {
			setErrorMessage(e.getMessage());
			return false;
		}

		return true;
	}

	/**
	 * Validate the discriminator value
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateDiscriminatorValueInput(String text) {
		final InheritanceTypeEnumeration inheritanceType = InheritanceTypeEnumeration
				.valueOf(cboInheritance.getItem(cboInheritance.getSelectionIndex()));

		if (inheritanceType != InheritanceTypeEnumeration.NONE) {
			if (text.isEmpty()) {
				setErrorMessage("The discriminator value must not be empty!");
				return false;
			}

			final String discType = cboDiscriminatorType.getItem(cboDiscriminatorType.getSelectionIndex());
			final DiscriminatorColumnTypeEnumeration type = DiscriminatorColumnTypeEnumeration.valueOf(discType);

			if (type == DiscriminatorColumnTypeEnumeration.CHAR && text.length() > 1) {
				setErrorMessage("The discriminator value must contain exactly one character!");
				return false;
			}

			if (type == DiscriminatorColumnTypeEnumeration.INTEGER) {
				try {
					Integer.parseInt(text);
				}
				catch (final NumberFormatException _) {
					setErrorMessage("The discriminator value requires an integer value!");
					return false;
				}
			}
		}

		return true;
	}

	/**
	 * Validate the discriminator column name
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateDiscriminatorColumnNameInput(String text) {
		final InheritanceTypeEnumeration inheritanceType = InheritanceTypeEnumeration
				.valueOf(cboInheritance.getItem(cboInheritance.getSelectionIndex()));

		if (inheritanceType == InheritanceTypeEnumeration.NONE)
			return true;

		final Database db = project.getDatabase();

		try {
			new DBSynchService(db).validateIdentifier(text);
		}
		catch (final DBObjectValidationException e) {
			setErrorMessage(e.getMessage());
			return false;
		}

		return true;
	}

	/**
	 * Validate the database identifier
	 * @param text
	 * @return true if the validation was successful
	 */
	private boolean validateDBIdentifier(String text) {
		final Database db = project.getDatabase();

		if (text.isEmpty())
			return true;

		try {
			new DBSynchService(db).validateIdentifier(text);
		}
		catch (final DBObjectValidationException e) {
			setErrorMessage(e.getMessage());
			return false;
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		newShell.setText("Create domain object");
		super.configureShell(newShell);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			final boolean ok = validateAllInputs();

			if (!ok)
				return;

			setDomainObjectData();
		}

		super.buttonPressed(buttonId);
	}

	/**
	 * Apply the input to the domain object
	 */
	private void setDomainObjectData() {
		final InheritanceTypeEnumeration inheritanceType = InheritanceTypeEnumeration
				.valueOf(cboInheritance.getItem(cboInheritance.getSelectionIndex()));
		final IDGeneratorTypeEnumeration generatorType = IDGeneratorTypeEnumeration
				.valueOf(cboIdGeneratorType.getItem(cboIdGeneratorType.getSelectionIndex()));

		domainObject.setAbstract(chkAbstract.getSelection());
		domainObject.setMappedSuperClass(chkMappedSuperClass.getSelection());
		domainObject.setNamespace(namespace);
		domainObject.setPropertyAccess(chkPropertyAccess.getSelection());
		domainObject.setName(txtDomainObjectName.getText());
		domainObject.setNamePlural(txtNamePlural.getText());
		domainObject.setLabel(txtLabel.getText());
		domainObject.setLabelPlural(txtLabelPlural.getText());
		domainObject.setDiscriminatorValue(txtDiscriminatorValue.getText());
		domainObject.setComment(txtComment.getText());
		domainObject.setTag(DomainTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex())));
		domainObject.setPrimitive(false);
		domainObject.setMappable(true);

		// Create the database table for the domain object
		final DBTable table = DbFactory.eINSTANCE.createDBTable();
		table.setName(txtTableName.getText());
		table.setDatabase(project.getDatabase());
		table.setSchemaName(txtSchemaName.getText());
		table.setCatalogName(txtCatalogName.getText());

		project.getDatabase().getDatabaseTables().add(table);
		domainObject.setDatabaseTable(table);

		// Create inheritance
		if (inheritanceType != InheritanceTypeEnumeration.NONE) {
			final DiscriminatorColumnTypeEnumeration discriminatorColumnType = DiscriminatorColumnTypeEnumeration
					.valueOf(cboDiscriminatorType.getItem(cboDiscriminatorType.getSelectionIndex()));

			domainObject.setDiscriminatorColumnType(discriminatorColumnType);
			domainObject.setDiscriminatorValue(txtDiscriminatorValue.getText());
			domainObject.setInheritanceType(inheritanceType);

			// Add a discriminator column. Note: Some vendors may not provide the JOINED-strategy without a discriminator column.
			final DBColumn discriminatorColumn = DbFactory.eINSTANCE.createDBColumn();
			domainObject.setDiscriminatorColumn(discriminatorColumn);
			table.getColumns().add(discriminatorColumn);
			boolean exit = false;

			for (final DBColumnType dct : project.getDatabase().getAllSupportedColumnTypes()) {
				for (final JavaType jt : dct.getJavaTypes()) {
					if (jt.getName().equalsIgnoreCase(discriminatorColumnType.toString())) {
						discriminatorColumn.setColumnType(dct);
						exit = true;
						break;
					}
				}

				if (exit)
					break;
			}

			domainObject.getDiscriminatorColumn().setName(txtColumnName.getText());

			if (discriminatorColumnType == DiscriminatorColumnTypeEnumeration.STRING)
				domainObject.getDiscriminatorColumn().setLength(255);

			domainObject.getDiscriminatorColumn().setNullable(false);
		}

		// Create an ID generator for the domain object
		final IDGenerator generator = DomainFactory.eINSTANCE.createIDGenerator();
		domainObject.setIDGenerator(generator);

		generator.setGeneratorType(generatorType);
		domainObject.getIDGenerator().setName(domainObject.getName().toUpperCase() + "_SEQ");

		if (!txtInitialValue.getText().isEmpty())
			domainObject.getIDGenerator().setInitialValue(Integer.parseInt(txtInitialValue.getText()));

		if (!txtBlockSize.getText().isEmpty())
			domainObject.getIDGenerator().setBlockSize(Integer.parseInt(txtBlockSize.getText()));

		domainObject.setNamespace(namespace);
		namespace.getJavaTypes().add(domainObject);
	}

}
