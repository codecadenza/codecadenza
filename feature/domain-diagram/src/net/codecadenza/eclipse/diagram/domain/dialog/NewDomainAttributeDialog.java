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
import static net.codecadenza.eclipse.shared.Constants.MIN_FILTER_LENGTH;

import java.util.Collection;
import java.util.HashMap;
import java.util.Set;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBColumnType;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.DbFactory;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionMappingStrategyEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeComparator;
import net.codecadenza.eclipse.model.project.PersistenceProviderEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.dbsync.util.DBObjectValidationException;
import net.codecadenza.eclipse.tools.dbsync.util.DBSynchService;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
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
 * Dialog for creating new domain attributes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class NewDomainAttributeDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Create new domain attribute";
	private static final int UUID_STRING_LENGTH = 36;
	private static final int UUID_LENGTH = 16;
	private static final Integer DEFAULT_STRING_LENGTH = 50;
	private static final Integer DEFAULT_BYTE_ARRAY_LENGTH = 65536;
	private static final int DEFAULT_CHAR_LENGTH = 1;

	private Text txtDBColumnScale;
	private Text txtDBColumnPrecision;
	private Combo cboDBColumnType;
	private Combo cboTag;
	private Text txtDBColumnName;
	private Text txtValidatorRegularExpression;
	private Text txtValidatorMaxValue;
	private Text txtValidatorMinValue;
	private Text txtValidatorMaxLength;
	private Text txtValidatorMinLength;
	private Combo cboAttributeTemporalType;
	private AbstractProposalTextField<JavaType> propType;
	private Text txtAttributeLabelPlural;
	private Text txtAttributeLabel;
	private Text txtAttributeName;
	private Button chkAttributePrimaryKey;
	private Button chkAttributePersistent;
	private Button chkAttributeInsertable;
	private Button chkAttributeUpdatable;
	private Button chkAttributeSetDateOnPersist;
	private Button chkAttributeSetDateOnUpdate;
	private Button chkAttributeFetchTypeEager;
	private Button chkValidatorFutureDate;
	private Button chkValidatorPastDate;
	private Button chkValidatorNullable;
	private Button chkAttributeVersion;
	private Button chkDisplayAttribute;
	private Button chkRemoveWhitespaceCharacters;
	private Button chkConvertToUpperCase;
	private Button chkConvertToLowerCase;
	private Combo cboCollectionType;
	private Combo cboCollectionStrategy;
	private Text txtCollectionTable;
	private final DomainAttribute domainAttribute;
	private final Project project;
	private final boolean enableElementCollection;
	private HashMap<String, DBColumnType> columnTypeMap = new HashMap<>();
	private Button chkUnique;
	private boolean unique;
	private String collectionTableName;

	/**
	 * Create the dialog
	 * @param parentShell
	 * @param domainAttribute
	 * @param project
	 */
	public NewDomainAttributeDialog(Shell parentShell, DomainAttribute domainAttribute, Project project) {
		super(parentShell);

		this.project = project;
		this.domainAttribute = domainAttribute;

		// Element collections are not supported for mapped superclasses!
		this.enableElementCollection = !domainAttribute.getDomainObject().isMappedSuperClass();
	}

	/**
	 * Disable all input fields
	 * @param resetCollectionType
	 */
	private void disableAllFields(boolean resetCollectionType) {
		cboTag.setEnabled(false);
		cboAttributeTemporalType.select(0);
		chkAttributeFetchTypeEager.setEnabled(false);
		chkAttributeFetchTypeEager.setSelection(false);
		chkAttributeInsertable.setEnabled(false);
		chkAttributeInsertable.setSelection(false);
		chkAttributePrimaryKey.setEnabled(false);
		chkAttributePrimaryKey.setSelection(false);
		chkAttributeSetDateOnPersist.setSelection(false);
		chkAttributeSetDateOnPersist.setEnabled(false);
		chkAttributeSetDateOnUpdate.setSelection(false);
		chkAttributeSetDateOnUpdate.setEnabled(false);
		chkAttributeUpdatable.setEnabled(false);
		chkAttributeUpdatable.setSelection(false);
		chkAttributeVersion.setEnabled(false);
		chkAttributeVersion.setSelection(false);
		chkDisplayAttribute.setEnabled(false);
		chkDisplayAttribute.setSelection(false);
		chkUnique.setEnabled(false);
		chkUnique.setSelection(false);
		chkValidatorFutureDate.setSelection(false);
		chkValidatorFutureDate.setEnabled(false);
		chkValidatorNullable.setSelection(false);
		chkValidatorNullable.setEnabled(false);
		chkValidatorPastDate.setEnabled(false);
		chkValidatorPastDate.setSelection(false);
		txtDBColumnName.setEnabled(false);
		txtDBColumnPrecision.setText("");
		txtDBColumnPrecision.setEnabled(false);
		txtDBColumnScale.setText("");
		txtDBColumnScale.setEnabled(false);
		txtValidatorMaxLength.setText("");
		txtValidatorMaxLength.setEnabled(false);
		txtValidatorMaxValue.setText("");
		txtValidatorMaxValue.setEnabled(false);
		txtValidatorMinLength.setText("");
		txtValidatorMinLength.setEnabled(false);
		txtValidatorMinValue.setText("");
		txtValidatorMinValue.setEnabled(false);
		txtValidatorRegularExpression.setText("");
		txtValidatorRegularExpression.setEnabled(false);
		cboAttributeTemporalType.setEnabled(false);
		cboDBColumnType.setEnabled(false);
		chkRemoveWhitespaceCharacters.setEnabled(false);
		chkRemoveWhitespaceCharacters.setSelection(false);
		chkConvertToUpperCase.setEnabled(false);
		chkConvertToUpperCase.setSelection(false);
		chkConvertToLowerCase.setEnabled(false);
		chkConvertToLowerCase.setSelection(false);
		cboTag.select(0);
		cboCollectionType.setEnabled(false);
		cboCollectionStrategy.setEnabled(false);
		txtCollectionTable.setText("");
		txtCollectionTable.setEnabled(false);

		if (resetCollectionType) {
			cboCollectionStrategy.removeAll();
			cboCollectionStrategy.add(CollectionMappingStrategyEnumeration.NONE.toString());
			cboCollectionStrategy.select(0);

			cboCollectionType.select(0);
		}
	}

	/**
	 * Enable fields that are commonly used by all types
	 * @param type
	 */
	private void enableFields(JavaType type) {
		// Set the selected column type
		cboDBColumnType.removeAll();
		columnTypeMap = new HashMap<>();

		type.getDBColumnTypes(project).forEach(colType -> {
			columnTypeMap.put(colType.getName(), colType);
			cboDBColumnType.add(colType.getName());
		});

		cboDBColumnType.select(0);

		cboTag.setEnabled(true);
		chkAttributeFetchTypeEager.setEnabled(true);
		chkAttributeFetchTypeEager.setSelection(true);
		chkAttributeInsertable.setEnabled(true);
		chkAttributeInsertable.setSelection(true);
		chkAttributeUpdatable.setEnabled(true);
		chkAttributeUpdatable.setSelection(true);
		chkUnique.setEnabled(true);
		txtDBColumnName.setEnabled(true);
		cboDBColumnType.setEnabled(true);

		if (!type.isPrimitive())
			chkValidatorNullable.setEnabled(true);

		setDefaultLengthConstraints(propType.getSelectedItem());
	}

	/**
	 * Enable fields for the type java.lang.String
	 * @param type
	 * @param collectionType
	 */
	private void enableFieldsForString(JavaType type, CollectionTypeEnumeration collectionType) {
		enableFields(type);

		txtValidatorMinLength.setEnabled(true);
		txtValidatorMaxLength.setEnabled(true);
		cboCollectionType.setEnabled(enableElementCollection);
		cboCollectionStrategy.setEnabled(collectionType != CollectionTypeEnumeration.NONE);
		chkAttributePrimaryKey.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		chkDisplayAttribute.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		txtValidatorRegularExpression.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		chkRemoveWhitespaceCharacters.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		chkRemoveWhitespaceCharacters.setSelection(collectionType == CollectionTypeEnumeration.NONE);
		chkConvertToUpperCase.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		chkConvertToLowerCase.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
	}

	/**
	 * Enable fields for all numeric types (e.g. int, Integer, double...)
	 * @param type
	 * @param collectionType
	 */
	private void enableFieldsForNumericTypes(JavaType type, CollectionTypeEnumeration collectionType) {
		enableFields(type);

		txtDBColumnPrecision.setEnabled(true);
		txtDBColumnScale.setEnabled(true);
		cboCollectionType.setEnabled(enableElementCollection);
		cboCollectionStrategy.setEnabled(collectionType != CollectionTypeEnumeration.NONE);
		txtValidatorMaxValue.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		txtValidatorMinValue.setEnabled(collectionType == CollectionTypeEnumeration.NONE);

		if (type.isIntegerOrLong()) {
			chkAttributePrimaryKey.setEnabled(collectionType == CollectionTypeEnumeration.NONE);

			// A whole number has no decimal places. Thus, we should omit the scale!
			txtDBColumnScale.setEnabled(false);
			txtDBColumnScale.setText("");
			chkAttributeVersion.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		}
	}

	/**
	 * Enable fields for all date types
	 * @param type
	 * @param collectionType
	 */
	private void enableFieldsForDateTypes(JavaType type, CollectionTypeEnumeration collectionType) {
		enableFields(type);

		cboCollectionType.setEnabled(enableElementCollection);
		cboCollectionStrategy.setEnabled(collectionType != CollectionTypeEnumeration.NONE);
		chkValidatorFutureDate.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		chkValidatorPastDate.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		chkAttributeSetDateOnPersist.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
		chkAttributeSetDateOnUpdate.setEnabled(collectionType == CollectionTypeEnumeration.NONE);

		if (type.isDateOrCalendar()) {
			cboAttributeTemporalType.setEnabled(true);
			cboAttributeTemporalType.select(1);
		}
		else {
			cboAttributeTemporalType.select(0);
			cboAttributeTemporalType.setEnabled(false);
		}
	}

	/**
	 * Enable fields for the char type
	 * @param type
	 * @param collectionType
	 */
	private void enableFieldsForCharType(JavaType type, CollectionTypeEnumeration collectionType) {
		enableFields(type);

		cboCollectionType.setEnabled(enableElementCollection);
		cboCollectionStrategy.setEnabled(collectionType != CollectionTypeEnumeration.NONE);
		txtValidatorMinLength.setEnabled(true);
	}

	/**
	 * Enable fields for all LOB types
	 * @param type
	 */
	private void enableFieldsForLobTypes(JavaType type) {
		enableFields(type);

		chkUnique.setEnabled(false);
		chkAttributeFetchTypeEager.setSelection(false);
		txtValidatorMinLength.setEnabled(true);
		txtValidatorMaxLength.setEnabled(true);
	}

	/**
	 * Enable fields that for all boolean types
	 * @param type
	 */
	private void enableFieldsForBooleanTypes(JavaType type) {
		enableFields(type);

		chkUnique.setEnabled(false);
	}

	/**
	 * Enable fields for the type java.util.UUID
	 * @param type
	 * @param collectionType
	 */
	private void enableFieldsForUUID(JavaType type, CollectionTypeEnumeration collectionType) {
		enableFields(type);

		cboCollectionType.setEnabled(enableElementCollection);
		cboCollectionStrategy.setEnabled(enableElementCollection);
		chkAttributePrimaryKey.setEnabled(collectionType == CollectionTypeEnumeration.NONE);
	}

	/**
	 * Set the default minimum and maximum length depending on the column and Java type
	 * @param selType
	 */
	private void setDefaultLengthConstraints(JavaType selType) {
		if (selType == null || !chkAttributePersistent.getSelection())
			return;

		Integer minLength = null;
		Integer maxLength = null;

		if (selType.isString() || selType.isByteArray()) {
			Integer enteredMaxLength = null;

			try {
				if (!txtValidatorMaxLength.getText().isEmpty())
					enteredMaxLength = Integer.parseInt(txtValidatorMaxLength.getText());
			}
			catch (final NumberFormatException e) {
				// Ignored!
			}

			// If the field can be null it's minimum length should be 0 in order allow an empty input!
			minLength = chkValidatorNullable.getSelection() ? 0 : 1;

			// Do not overwrite a valid maximum length entered by the user!
			if (selType.isString())
				maxLength = enteredMaxLength == null ? DEFAULT_STRING_LENGTH : enteredMaxLength;
			else
				maxLength = enteredMaxLength == null ? DEFAULT_BYTE_ARRAY_LENGTH : enteredMaxLength;
		}
		else if (selType.isUUID()) {
			final int colTypeIndex = cboDBColumnType.getSelectionIndex();

			if (colTypeIndex > 0) {
				final DBColumnType columnType = columnTypeMap.get(cboDBColumnType.getItem(cboDBColumnType.getSelectionIndex()));

				maxLength = UUID_LENGTH;

				if (columnType.getJavaTypes().stream().anyMatch(type -> type.getName().equals(JavaType.STRING)))
					maxLength = UUID_STRING_LENGTH;
			}
			else
				maxLength = DEFAULT_STRING_LENGTH;
		}
		else if (selType.isChar()) {
			minLength = DEFAULT_CHAR_LENGTH;
			maxLength = minLength;
		}

		if (maxLength != null)
			txtValidatorMaxLength.setText(Integer.toString(maxLength));

		if (minLength != null)
			txtValidatorMinLength.setText(Integer.toString(minLength));
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent);

		final var groupBasicData = new Group(panDialogArea, SWT.NONE);
		groupBasicData.setText("Basic data");
		groupBasicData.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		groupBasicData.setLayout(new GridLayout(4, false));

		final var lblAttributeName = new Label(groupBasicData, SWT.NONE);
		lblAttributeName.setText("Name:");

		final var gdAttributeName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdAttributeName.widthHint = 200;

		txtAttributeName = new Text(groupBasicData, SWT.BORDER);
		txtAttributeName.setLayoutData(gdAttributeName);

		txtAttributeName.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				final CollectionTypeEnumeration selectedType = CollectionTypeEnumeration
						.valueOf(cboCollectionType.getItem(cboCollectionType.getSelectionIndex()));

				txtAttributeLabel.setText(EclipseIDEService.buildDefaultLabel(txtAttributeName.getText()));

				if (selectedType == CollectionTypeEnumeration.NONE)
					txtAttributeLabelPlural.setText(txtAttributeLabel.getText());
				else
					txtAttributeLabelPlural.setText(EclipseIDEService.buildDefaultPluralLabel(txtAttributeName.getText()));

				txtDBColumnName.setText(EclipseIDEService.buildDefaultColumnName(txtAttributeName.getText()));
			}
		});

		final var lblAttributeJavaType = new Label(groupBasicData, SWT.NONE);
		lblAttributeJavaType.setText("Java type:");

		propType = new AbstractProposalTextField<>(groupBasicData, SWT.BORDER, MIN_FILTER_LENGTH) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
			 */
			@Override
			public Collection<JavaType> getProposalData(String filter) {
				final var types = new BasicEList<JavaType>();

				for (final JavaType type : project.getAllSupportedTypes())
					if (type.isMappable() && type.getName().toLowerCase().startsWith(filter.toLowerCase()))
						types.add(type);

				ECollections.sort(types, new JavaTypeComparator());

				return types;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang. Object)
			 */
			@Override
			public String getProposalLabel(JavaType element) {
				return element.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang. Object)
			 */
			@Override
			public void onProposalAccepted(JavaType element) {
				initOnTypeChange(true);
			}
		};

		final var gdType = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdType.widthHint = 200;

		propType.setLayoutData(gdType);

		final var lblAttributeLabel = new Label(groupBasicData, SWT.NONE);
		lblAttributeLabel.setText("Label:");

		txtAttributeLabel = new Text(groupBasicData, SWT.BORDER);
		txtAttributeLabel.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		txtAttributeLabel.addFocusListener(new FocusAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.FocusAdapter#focusLost(org.eclipse.swt.events.FocusEvent)
			 */
			@Override
			public void focusLost(FocusEvent e) {
				final CollectionTypeEnumeration selectedType = CollectionTypeEnumeration
						.valueOf(cboCollectionType.getItem(cboCollectionType.getSelectionIndex()));

				if (selectedType == CollectionTypeEnumeration.NONE)
					txtAttributeLabelPlural.setText(txtAttributeLabel.getText());
				else
					txtAttributeLabelPlural.setText(EclipseIDEService.buildDefaultPluralForm(txtAttributeLabel.getText()));

				txtDBColumnName.setText(txtAttributeLabel.getText().toLowerCase().replace(" ", "_").replace(".", ""));
			}
		});

		final var lblAttributeLabelPlural = new Label(groupBasicData, SWT.NONE);
		lblAttributeLabelPlural.setText("Label plural:");

		txtAttributeLabelPlural = new Text(groupBasicData, SWT.BORDER);
		txtAttributeLabelPlural.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblAttributePrimaryKey = new Label(groupBasicData, SWT.NONE);
		lblAttributePrimaryKey.setText("Primary key:");

		chkAttributePrimaryKey = new Button(groupBasicData, SWT.CHECK);

		final var lblAttributePersistent = new Label(groupBasicData, SWT.NONE);
		lblAttributePersistent.setText("Persistent:");

		chkAttributePersistent = new Button(groupBasicData, SWT.CHECK);

		chkAttributePersistent.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				if (!chkAttributePersistent.getSelection()) {
					disableAllFields(true);
					return;
				}

				initOnTypeChange(true);
			}
		});

		chkAttributePersistent.setSelection(true);

		final var lblAttributeInsertable = new Label(groupBasicData, SWT.NONE);
		lblAttributeInsertable.setText("Insertable:");

		chkAttributeInsertable = new Button(groupBasicData, SWT.CHECK);
		chkAttributeInsertable.setSelection(true);

		final var lblAttributeUpdatable = new Label(groupBasicData, SWT.NONE);
		lblAttributeUpdatable.setText("Updatable:");

		chkAttributeUpdatable = new Button(groupBasicData, SWT.CHECK);
		chkAttributeUpdatable.setSelection(true);

		final var lblAttributeSetDateOnPersist = new Label(groupBasicData, SWT.NONE);
		lblAttributeSetDateOnPersist.setText("Set date on persist:");

		chkAttributeSetDateOnPersist = new Button(groupBasicData, SWT.CHECK);

		chkAttributeSetDateOnPersist.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (chkAttributeSetDateOnPersist.getSelection()) {
					chkAttributeSetDateOnUpdate.setSelection(false);
					chkAttributeInsertable.setSelection(true);
					chkAttributeUpdatable.setSelection(false);
					chkValidatorFutureDate.setSelection(false);
					chkValidatorPastDate.setSelection(false);
					chkAttributeInsertable.setEnabled(false);
					chkAttributeUpdatable.setEnabled(false);
					chkValidatorFutureDate.setEnabled(false);
					chkValidatorPastDate.setEnabled(false);
				}
				else {
					chkAttributeInsertable.setEnabled(true);
					chkAttributeUpdatable.setEnabled(true);
					chkValidatorFutureDate.setEnabled(true);
					chkValidatorPastDate.setEnabled(true);
				}
			}
		});

		final var lblAttributeSetDateOnUpdate = new Label(groupBasicData, SWT.NONE);
		lblAttributeSetDateOnUpdate.setText("Set date on update:");

		chkAttributeSetDateOnUpdate = new Button(groupBasicData, SWT.CHECK);

		chkAttributeSetDateOnUpdate.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				if (chkAttributeSetDateOnUpdate.getSelection()) {
					chkAttributeSetDateOnPersist.setSelection(false);
					chkAttributeInsertable.setSelection(true);
					chkAttributeUpdatable.setSelection(true);
					chkValidatorFutureDate.setSelection(false);
					chkValidatorPastDate.setSelection(false);
					chkAttributeInsertable.setEnabled(false);
					chkAttributeUpdatable.setEnabled(false);
					chkValidatorFutureDate.setEnabled(false);
					chkValidatorPastDate.setEnabled(false);
					chkValidatorNullable.setSelection(true);
				}
				else {
					chkAttributeInsertable.setEnabled(true);
					chkAttributeUpdatable.setEnabled(true);
					chkValidatorFutureDate.setEnabled(true);
					chkValidatorPastDate.setEnabled(true);
					chkValidatorNullable.setSelection(false);
				}
			}
		});

		final var lblAttributeFetchTypeEager = new Label(groupBasicData, SWT.NONE);
		lblAttributeFetchTypeEager.setText("Fetch type eager:");

		chkAttributeFetchTypeEager = new Button(groupBasicData, SWT.CHECK);
		chkAttributeFetchTypeEager.setSelection(true);

		final var lblAttributeTemporalType = new Label(groupBasicData, SWT.NONE);
		lblAttributeTemporalType.setText("Temporal type:");

		cboAttributeTemporalType = new Combo(groupBasicData, SWT.READ_ONLY);
		cboAttributeTemporalType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final TemporalTypeEnumeration type : TemporalTypeEnumeration.values())
			cboAttributeTemporalType.add(type.toString());

		final var lblVersion = new Label(groupBasicData, SWT.NONE);
		lblVersion.setText("Version:");

		chkAttributeVersion = new Button(groupBasicData, SWT.CHECK);

		final var lblDisplayAttribute = new Label(groupBasicData, SWT.NONE);
		lblDisplayAttribute.setText("Display attribute:");

		chkDisplayAttribute = new Button(groupBasicData, SWT.CHECK);

		chkDisplayAttribute.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				chkUnique.setSelection(chkDisplayAttribute.getSelection());

				if (chkDisplayAttribute.getSelection()) {
					chkValidatorNullable.setSelection(false);
					chkValidatorNullable.setEnabled(false);
				}
				else
					chkValidatorNullable.setEnabled(true);
			}
		});

		final var lblTag = new Label(groupBasicData, SWT.NONE);
		lblTag.setText("Tag:");

		cboTag = new Combo(groupBasicData, SWT.READ_ONLY);
		cboTag.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		domainAttribute.getDomainObject().getValidAttributeTags().forEach(cboTag::add);

		if (cboTag.getItemCount() > 1)
			cboTag.select(1);
		else
			cboTag.select(0);

		final var tabFolderAddData = new TabFolder(panDialogArea, SWT.NONE);
		tabFolderAddData.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var tabItemDBMapping = new TabItem(tabFolderAddData, SWT.NONE);
		tabItemDBMapping.setText("Database mapping");

		final var panDBMapping = new Composite(tabFolderAddData, SWT.NONE);
		panDBMapping.setLayout(new GridLayout(4, false));

		tabItemDBMapping.setControl(panDBMapping);

		final var lblDBColumnName = new Label(panDBMapping, SWT.NONE);
		lblDBColumnName.setText("Column name:");

		txtDBColumnName = new Text(panDBMapping, SWT.BORDER);
		txtDBColumnName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblDBColumnType = new Label(panDBMapping, SWT.NONE);
		lblDBColumnType.setText("Column type:");

		cboDBColumnType = new Combo(panDBMapping, SWT.READ_ONLY);
		cboDBColumnType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		cboDBColumnType.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				setDefaultLengthConstraints(propType.getSelectedItem());
			}
		});

		final var lblDBColumnPrecision = new Label(panDBMapping, SWT.NONE);
		lblDBColumnPrecision.setText("Precision:");

		txtDBColumnPrecision = new Text(panDBMapping, SWT.BORDER);
		txtDBColumnPrecision.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblDBColumnScale = new Label(panDBMapping, SWT.NONE);
		lblDBColumnScale.setText("Scale:");

		txtDBColumnScale = new Text(panDBMapping, SWT.BORDER);
		txtDBColumnScale.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblUnique = new Label(panDBMapping, SWT.NONE);
		lblUnique.setText("Unique:");

		chkUnique = new Button(panDBMapping, SWT.CHECK);

		chkUnique.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				if (chkUnique.getSelection()) {
					chkValidatorNullable.setSelection(false);
					chkValidatorNullable.setEnabled(false);
				}
				else
					chkValidatorNullable.setEnabled(true);
			}
		});

		final var tabItemValidation = new TabItem(tabFolderAddData, SWT.NONE);
		tabItemValidation.setText("Validation");

		final var panValidation = new Composite(tabFolderAddData, SWT.NONE);
		panValidation.setLayout(new GridLayout(4, false));

		tabItemValidation.setControl(panValidation);

		final var lblValidatorFutureDate = new Label(panValidation, SWT.NONE);
		lblValidatorFutureDate.setText("Future date:");

		chkValidatorFutureDate = new Button(panValidation, SWT.CHECK);

		final var lblValidatorPastDate = new Label(panValidation, SWT.NONE);
		lblValidatorPastDate.setText("Past date:");

		chkValidatorPastDate = new Button(panValidation, SWT.CHECK);

		final var lblValidatorMinLength = new Label(panValidation, SWT.NONE);
		lblValidatorMinLength.setText("Min. length:");

		txtValidatorMinLength = new Text(panValidation, SWT.BORDER);
		txtValidatorMinLength.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorMaxLength = new Label(panValidation, SWT.NONE);
		lblValidatorMaxLength.setText("Max. length:");

		txtValidatorMaxLength = new Text(panValidation, SWT.BORDER);
		txtValidatorMaxLength.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorMinValue = new Label(panValidation, SWT.NONE);
		lblValidatorMinValue.setText("Min. value:");

		txtValidatorMinValue = new Text(panValidation, SWT.BORDER);
		txtValidatorMinValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorMaxValue = new Label(panValidation, SWT.NONE);
		lblValidatorMaxValue.setText("Max. value:");

		txtValidatorMaxValue = new Text(panValidation, SWT.BORDER);
		txtValidatorMaxValue.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorRegularExpression = new Label(panValidation, SWT.NONE);
		lblValidatorRegularExpression.setText("Regular expression:");

		txtValidatorRegularExpression = new Text(panValidation, SWT.BORDER);
		txtValidatorRegularExpression.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblValidatorNullable = new Label(panValidation, SWT.NONE);
		lblValidatorNullable.setText("Nullable:");

		chkValidatorNullable = new Button(panValidation, SWT.CHECK);

		chkValidatorNullable.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				setDefaultLengthConstraints(propType.getSelectedItem());
			}
		});

		final var tabItemConversion = new TabItem(tabFolderAddData, SWT.NONE);
		tabItemConversion.setText("Conversion");

		final var panConversion = new Composite(tabFolderAddData, SWT.NONE);
		panConversion.setLayout(new GridLayout(2, false));

		tabItemConversion.setControl(panConversion);

		final var lblRemoveWhitespaceChars = new Label(panConversion, SWT.NONE);
		lblRemoveWhitespaceChars.setText("Remove whitespace characters:");

		chkRemoveWhitespaceCharacters = new Button(panConversion, SWT.CHECK);

		final var lblConvertToUpperCase = new Label(panConversion, SWT.NONE);
		lblConvertToUpperCase.setText("Convert to upper-case:");

		chkConvertToUpperCase = new Button(panConversion, SWT.CHECK);

		chkConvertToUpperCase.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				// Avoid applying a useless configuration!
				if (chkConvertToUpperCase.getSelection())
					chkConvertToLowerCase.setSelection(false);
			}
		});

		final var lblConvertToLowerCase = new Label(panConversion, SWT.NONE);
		lblConvertToLowerCase.setText("Convert to lower-case:");

		chkConvertToLowerCase = new Button(panConversion, SWT.CHECK);

		chkConvertToLowerCase.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				// Avoid applying a useless configuration!
				if (chkConvertToLowerCase.getSelection())
					chkConvertToUpperCase.setSelection(false);
			}
		});

		final var tabItemCollection = new TabItem(tabFolderAddData, SWT.NONE);
		tabItemCollection.setText("Element collection");

		final var panCollection = new Composite(tabFolderAddData, SWT.NONE);
		panCollection.setLayout(new GridLayout(2, false));

		tabItemCollection.setControl(panCollection);

		final var lblCollectionType = new Label(panCollection, SWT.NONE);
		lblCollectionType.setText("Collection type:");

		cboCollectionType = new Combo(panCollection, SWT.READ_ONLY);
		cboCollectionType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		for (final CollectionTypeEnumeration type : CollectionTypeEnumeration.values())
			cboCollectionType.add(type.toString());

		cboCollectionType.select(0);

		cboCollectionType.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				final CollectionTypeEnumeration selectedCollectionType = CollectionTypeEnumeration
						.valueOf(cboCollectionType.getItem(cboCollectionType.getSelectionIndex()));

				initOnTypeChange(false);

				cboCollectionStrategy.removeAll();

				if (selectedCollectionType != CollectionTypeEnumeration.NONE) {
					if (project.getPersistenceProvider() != PersistenceProviderEnumeration.ECLIPSELINK)
						cboCollectionStrategy.add(CollectionMappingStrategyEnumeration.CONVERTER.toString());

					cboCollectionStrategy.add(CollectionMappingStrategyEnumeration.TABLE.toString());

					txtAttributeLabelPlural.setText(txtAttributeLabel.getText());
				}
				else {
					cboCollectionStrategy.add(CollectionMappingStrategyEnumeration.NONE.toString());

					txtAttributeLabelPlural.setText(EclipseIDEService.buildDefaultPluralLabel(txtAttributeName.getText()));
				}

				cboCollectionStrategy.select(0);

				changeCollectionStrategy();
			}
		});

		final var lblCollectionStrategy = new Label(panCollection, SWT.NONE);
		lblCollectionStrategy.setText("Collection strategy:");

		cboCollectionStrategy = new Combo(panCollection, SWT.READ_ONLY);
		cboCollectionStrategy.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboCollectionStrategy.setEnabled(false);

		cboCollectionStrategy.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(final SelectionEvent e) {
				changeCollectionStrategy();
			}
		});

		cboCollectionStrategy.add(CollectionMappingStrategyEnumeration.NONE.toString());

		final var lblCollectionTable = new Label(panCollection, SWT.NONE);
		lblCollectionTable.setText("Collection table name:");

		txtCollectionTable = new Text(panCollection, SWT.BORDER);
		txtCollectionTable.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtCollectionTable.setEnabled(false);

		txtAttributeName.setText("newAttribute");
		txtAttributeName.setSelection(0, txtAttributeName.getText().length());
		txtAttributeName.setFocus();

		// We disable all fields initially!
		disableAllFields(true);

		return panDialogArea;
	}

	/**
	 * Enable all respective fields depending on the selected attribute type
	 * @param resetCollectionType
	 */
	private void initOnTypeChange(boolean resetCollectionType) {
		final JavaType type = propType.getSelectedItem();
		final CollectionTypeEnumeration selectedCollectionType = CollectionTypeEnumeration
				.valueOf(cboCollectionType.getItem(cboCollectionType.getSelectionIndex()));

		disableAllFields(resetCollectionType);

		if (type == null)
			return;

		cboDBColumnType.removeAll();
		chkAttributePersistent.setSelection(true);

		if (type.isTemporalType())
			enableFieldsForDateTypes(type, selectedCollectionType);
		else if (type.isString())
			enableFieldsForString(type, selectedCollectionType);
		else if (type.isNumber())
			enableFieldsForNumericTypes(type, selectedCollectionType);
		else if (type.isChar())
			enableFieldsForCharType(type, selectedCollectionType);
		else if (type.isByteArray())
			enableFieldsForLobTypes(type);
		else if (type.isBoolean())
			enableFieldsForBooleanTypes(type);
		else if (type.isUUID())
			enableFieldsForUUID(type, selectedCollectionType);
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

	/**
	 * Get the attribute
	 * @return the domain attribute
	 */
	public DomainAttribute getDomainAttribute() {
		return domainAttribute;
	}

	/**
	 * @return true if the attribute should provide a unique key
	 */
	public boolean isUnique() {
		return unique;
	}

	/**
	 * @return the name of the collection table
	 */
	public String getCollectionTableName() {
		return collectionTableName;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID && !writeInputsToObject())
			return;

		super.buttonPressed(buttonId);
	}

	/**
	 * Apply the input
	 * @return true if the validation was successful
	 */
	private boolean writeInputsToObject() {
		final IStatus status = EclipseIDEService.validateFieldName(txtAttributeName.getText());
		final JavaType selType = propType.getSelectedItem();
		final AttributeTagEnumeration attributeTag = AttributeTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex()));
		final CollectionTypeEnumeration collectionType = CollectionTypeEnumeration
				.valueOf(cboCollectionType.getItem(cboCollectionType.getSelectionIndex()));
		final CollectionMappingStrategyEnumeration selectedStrategy = CollectionMappingStrategyEnumeration
				.valueOf(cboCollectionStrategy.getItem(cboCollectionStrategy.getSelectionIndex()));
		boolean isLob = false;

		if (status.getSeverity() > IStatus.INFO) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
			return false;
		}

		domainAttribute.setName(txtAttributeName.getText());

		if (txtAttributeLabel.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The label must not be empty!");
			return false;
		}

		domainAttribute.setLabel(txtAttributeLabel.getText());

		if (txtAttributeLabelPlural.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The plural form of the label must not be empty!");
			return false;
		}

		domainAttribute.setLabelPlural(txtAttributeLabelPlural.getText());

		if (selType == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "A valid Java type must be selected!");
			propType.getControl().setFocus();
			return false;
		}

		// This check is redundant as the dialog should prevent us from setting primitive types to be nullable!
		if (selType.isPrimitive() && chkValidatorNullable.getSelection()
				&& selectedStrategy != CollectionMappingStrategyEnumeration.CONVERTER) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "A primitive type cannot be nullable!");
			return false;
		}

		if (selType.isByteArray())
			isLob = true;

		// Check if the type is valid for the selected tag!
		if (attributeTag != AttributeTagEnumeration.NONE) {
			final Set<JavaType> validTypes = CodeCadenzaResourcePlugin.getValidJavaTypesOfTag(project,
					AttributeTagEnumeration.valueOf(cboTag.getItem(cboTag.getSelectionIndex())));

			if (!validTypes.contains(selType)) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The attribute tag is not allowed for the selected type!");
				return false;
			}
		}

		if (collectionType != CollectionTypeEnumeration.NONE) {
			if (selectedStrategy == CollectionMappingStrategyEnumeration.NONE) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "A valid element collection strategy must be selected!");
				return false;
			}
			else if (selectedStrategy == CollectionMappingStrategyEnumeration.TABLE) {
				final Database db = project.getDatabase();

				if (project.getPersistenceProvider() == PersistenceProviderEnumeration.ECLIPSELINK
						&& (selType.isChar() || selType.isCalendar())) {
					// Internally, EclipseLink uses different types (e.g. String instead of Character) which cause ClassCastExceptions when
					// trying to iterate over the elements of the collection!
					final var message = "The type '" + selType.getName() + "' is not supported for an element collection!";
					MessageDialog.openInformation(getShell(), DLG_TITLE, message);
					return false;
				}

				try {
					new DBSynchService(db).validateTableName(db.getSchemaName(), db.getCatalogName(), txtCollectionTable.getText());
				}
				catch (final DBObjectValidationException e) {
					MessageDialog.openInformation(getShell(), DLG_TITLE, e.getMessage());
					return false;
				}
			}
		}

		domainAttribute.setJavaType(selType);
		domainAttribute.setFetchTypeEager(chkAttributeFetchTypeEager.getSelection());
		domainAttribute.setInsertable(chkAttributeInsertable.getSelection());
		domainAttribute.setPersistent(chkAttributePersistent.getSelection());
		domainAttribute.setPk(chkAttributePrimaryKey.getSelection());
		domainAttribute.setTag(attributeTag);

		unique = chkUnique.getSelection();
		collectionTableName = txtCollectionTable.getText();

		domainAttribute.setSetDateOnPersist(chkAttributeSetDateOnPersist.getSelection());
		domainAttribute.setSetDateOnUpdate(chkAttributeSetDateOnUpdate.getSelection());
		domainAttribute.setTemporalType(
				TemporalTypeEnumeration.valueOf(cboAttributeTemporalType.getItem(cboAttributeTemporalType.getSelectionIndex())));
		domainAttribute.setUpdatable(chkAttributeUpdatable.getSelection());
		domainAttribute.setTrackVersion(chkAttributeVersion.getSelection());
		domainAttribute.setDisplayAttribute(chkDisplayAttribute.getSelection());
		domainAttribute.setRemoveWhitespaceCharacters(chkRemoveWhitespaceCharacters.getSelection());
		domainAttribute.setConvertToUpperCase(chkConvertToUpperCase.getSelection());
		domainAttribute.setConvertToLowerCase(chkConvertToLowerCase.getSelection());

		if (domainAttribute.getTag() == AttributeTagEnumeration.CLIENT_DISPLAY)
			domainAttribute.setDisplayAttribute(true);

		domainAttribute.setLob(isLob);
		domainAttribute.setCollectionType(collectionType);
		domainAttribute.setCollectionMappingStrategy(selectedStrategy);

		final DomainAttributeValidator validator = DomainFactory.eINSTANCE.createDomainAttributeValidator();

		domainAttribute.setDomainAttributeValidator(validator);
		domainAttribute.getDomainAttributeValidator().setFutureDate(chkValidatorFutureDate.getSelection());
		domainAttribute.getDomainAttributeValidator().setNullable(chkValidatorNullable.getSelection());
		domainAttribute.getDomainAttributeValidator().setPastDate(chkValidatorPastDate.getSelection());
		domainAttribute.getDomainAttributeValidator().setMaxValue(txtValidatorMaxValue.getText());
		domainAttribute.getDomainAttributeValidator().setMinValue(txtValidatorMinValue.getText());
		domainAttribute.getDomainAttributeValidator().setRegularExpression(txtValidatorRegularExpression.getText());

		if (chkAttributePersistent.getSelection()) {
			final DBColumn column = DbFactory.eINSTANCE.createDBColumn();

			domainAttribute.setColumn(column);

			try {
				final Database db = project.getDatabase();
				final var dbSyncService = new DBSynchService(db);

				if (selectedStrategy != CollectionMappingStrategyEnumeration.TABLE) {
					DBTable table = domainAttribute.getDomainObject().getDatabaseTable();

					if (table == null)
						table = domainAttribute.getDomainObject().getRootParentDomainObject(false).getDatabaseTable();

					dbSyncService.validateColumnName(table, txtDBColumnName.getText());
				}
				else
					dbSyncService.validateIdentifier(txtDBColumnName.getText());
			}
			catch (final DBObjectValidationException ex) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, ex.getMessage());

				txtDBColumnName.setFocus();
				return false;
			}

			column.setName(txtDBColumnName.getText());

			if (cboDBColumnType.getSelectionIndex() == -1) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "A database column type must be selected!");
				return false;
			}

			if (selectedStrategy == CollectionMappingStrategyEnumeration.CONVERTER)
				column.setNullable(true);
			else
				column.setNullable(domainAttribute.getDomainAttributeValidator().isNullable());

			column.setColumnType(columnTypeMap.get(cboDBColumnType.getItem(cboDBColumnType.getSelectionIndex())));

			Integer minLength = null;
			Integer maxLength = null;

			try {
				if (txtValidatorMinLength.isEnabled() && !txtValidatorMinLength.getText().isEmpty()) {
					minLength = Integer.parseInt(txtValidatorMinLength.getText());

					if (minLength < 0)
						throw new IllegalArgumentException();

					domainAttribute.getDomainAttributeValidator().setMinLength(minLength);
				}
			}
			catch (final RuntimeException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The min. length requires a positive integer value!");
				return false;
			}

			if (selType.isDateOrCalendar() && domainAttribute.getTemporalType() == TemporalTypeEnumeration.NONE) {
				final var messageText = "Attributes of type java.util.Date or java.util.GregorianCalendar must be supplied with a valid temporal type!";

				MessageDialog.openInformation(getShell(), DLG_TITLE, messageText);
				return false;
			}

			try {
				final boolean requiresLength = !column.getColumnType().isOmitSizeInformation();

				if (txtValidatorMaxLength.isEnabled() && (requiresLength || !txtValidatorMaxLength.getText().isEmpty())) {
					maxLength = Integer.parseInt(txtValidatorMaxLength.getText());

					if (maxLength < 1)
						throw new IllegalArgumentException();

					if (requiresLength)
						column.setLength(maxLength);

					domainAttribute.getDomainAttributeValidator().setMaxLength(maxLength);
				}
				else if (domainAttribute.getJavaType().isUUID() && requiresLength)
					column.setLength(Integer.parseInt(txtValidatorMaxLength.getText()));
			}
			catch (final RuntimeException e) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The max. length requires a positive integer value!");
				return false;
			}

			// Check if the maximum length is greater than the minimum length
			if (maxLength != null && minLength != null && minLength > maxLength) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The max. length must be greater than the min. length!");
				return false;
			}

			if (!txtDBColumnPrecision.getText().isEmpty()) {
				int scale = 0;
				int precision = 0;

				try {
					precision = Integer.parseInt(txtDBColumnPrecision.getText());

					// Only positive values are allowed!
					if (precision < 1)
						throw new IllegalArgumentException();

					column.setPrecision(precision);
				}
				catch (final RuntimeException e) {
					MessageDialog.openInformation(getShell(), DLG_TITLE, "The precision requires a positive integer value!");
					return false;
				}

				if (!txtDBColumnScale.getText().isEmpty()) {
					try {
						scale = Integer.parseInt(txtDBColumnScale.getText());

						// Only positive values are allowed!
						if (scale < 1)
							throw new IllegalArgumentException();

						column.setScale(scale);
					}
					catch (final RuntimeException e) {
						MessageDialog.openInformation(getShell(), DLG_TITLE, "The scale requires a positive integer value!");
						return false;
					}
				}

				// Definition of precision and scale: e.g. 123.34: Precision = 5, Scale = 2!
				if (scale != 0 && scale > precision) {
					MessageDialog.openInformation(getShell(), DLG_TITLE, "The scale must be less or equal than the precision!");
					return false;
				}
			}
		}

		return true;
	}

	/**
	 * Update all necessary fields when the collection strategy has been changed
	 */
	private void changeCollectionStrategy() {
		final CollectionMappingStrategyEnumeration selectedStrategy = CollectionMappingStrategyEnumeration
				.valueOf(cboCollectionStrategy.getItem(cboCollectionStrategy.getSelectionIndex()));
		final JavaType selectedType = propType.getSelectedItem();

		cboDBColumnType.removeAll();
		columnTypeMap = new HashMap<>();

		selectedType.getDBColumnTypes(project).forEach(colType -> {
			columnTypeMap.put(colType.getName(), colType);
			cboDBColumnType.add(colType.getName());
		});

		setDefaultLengthConstraints(selectedType);

		txtValidatorMinLength.setEnabled(selectedType.isString() || selectedType.isByteArray());
		txtValidatorMaxLength.setEnabled(selectedType.isString() || selectedType.isByteArray());
		chkValidatorNullable.setEnabled(!selectedType.isPrimitive());
		txtCollectionTable.setText("");
		txtCollectionTable.setEnabled(false);

		if (selectedStrategy == CollectionMappingStrategyEnumeration.CONVERTER) {
			final JavaType stringType = project.getJavaTypeByName(JavaType.STRING);
			cboDBColumnType.removeAll();
			columnTypeMap = new HashMap<>();

			stringType.getDBColumnTypes(project).forEach(colType -> {
				columnTypeMap.put(colType.getName(), colType);
				cboDBColumnType.add(colType.getName());
			});

			txtValidatorMinLength.setEnabled(true);
			txtValidatorMaxLength.setEnabled(true);
			chkValidatorNullable.setEnabled(false);
			chkValidatorNullable.setSelection(false);

			setDefaultLengthConstraints(stringType);
		}
		else if (selectedStrategy == CollectionMappingStrategyEnumeration.TABLE) {
			final String defaultCollectionTableName = domainAttribute.getDomainObject().getLabel().replace(" ", "_") + "_"
					+ EclipseIDEService.buildDefaultColumnName(txtAttributeName.getText()) + DB_TABLE_SUFFIX;

			chkValidatorNullable.setEnabled(false);
			chkValidatorNullable.setSelection(false);
			txtCollectionTable.setEnabled(true);
			txtCollectionTable.setText(defaultCollectionTableName);
		}

		cboDBColumnType.select(0);
	}

}
