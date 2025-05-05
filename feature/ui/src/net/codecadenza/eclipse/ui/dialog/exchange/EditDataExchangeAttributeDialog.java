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
package net.codecadenza.eclipse.ui.dialog.exchange;

import static net.codecadenza.eclipse.shared.Constants.DEFAULT_VALUE_FLAG;
import static net.codecadenza.eclipse.shared.Constants.MIN_FILTER_LENGTH;

import java.math.BigDecimal;
import java.util.Collection;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeComparator;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining data exchange attributes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDataExchangeAttributeDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit data exchange attribute";
	private static final String DLG_TITLE_NEW = "Create new data exchange attribute";

	private Text txtName;
	private Text txtMappedName;
	private Text txtOrder;
	private Text txtSelectionListStatement;
	private Text txtType;
	private Text txtDefault;
	private Button chkDefault;
	private Combo cboDefaultEnumLiteral;
	private Button chkInsertable;
	private Button chkUpdatable;
	private Button chkOptional;
	private Button chkVisible;
	private Button chkReadonly;
	private Button chkDisableExtMapping;
	private Button chkCustomQuery;
	private List listValueListEntries;
	private DataExchangeAttribute dataExchangeAttribute;
	private boolean isMapped;
	private boolean isMappedToDomainAttribute;
	private boolean isMappedToDomainAssociation;
	private boolean editMode;
	private final ContentTypeEnumeration contentType;
	private final DataExchangeMethodTypeEnumeration methodType;
	private DataExchangeElement parentElement;
	private AbstractProposalTextField<JavaType> propType;
	private Project project;
	private Composite panDialogArea;
	private Text txtFormat;
	private final String title;
	private boolean mappedToElementCollection;

	/**
	 * Constructor
	 * @param parentShell
	 * @param parentElement
	 * @param project
	 * @param methodType
	 * @param contentType
	 */
	public EditDataExchangeAttributeDialog(Shell parentShell, DataExchangeElement parentElement, Project project,
			DataExchangeMethodTypeEnumeration methodType, ContentTypeEnumeration contentType) {
		super(parentShell);

		this.parentElement = parentElement;
		this.project = project;
		this.contentType = contentType;
		this.methodType = methodType;
		this.title = DLG_TITLE_NEW;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param exchangeAttribute
	 * @param methodType
	 * @param contentType
	 */
	public EditDataExchangeAttributeDialog(Shell parentShell, DataExchangeAttribute exchangeAttribute,
			DataExchangeMethodTypeEnumeration methodType, ContentTypeEnumeration contentType) {
		super(parentShell);

		this.dataExchangeAttribute = exchangeAttribute;
		this.editMode = true;
		this.contentType = contentType;
		this.methodType = methodType;
		this.title = DLG_TITLE_EDIT;

		if (exchangeAttribute.getMappingAttribute() != null) {
			isMapped = true;

			if (exchangeAttribute.getMappingAttribute().getDomainAttribute() != null) {
				final DomainAttribute domainAttribute = exchangeAttribute.getMappingAttribute().getDomainAttribute();
				final AbstractDomainAssociation assoc = exchangeAttribute.getMappingAttribute().getAssociation();

				isMappedToDomainAttribute = true;
				mappedToElementCollection = domainAttribute.getCollectionType() != CollectionTypeEnumeration.NONE;

				// In case of import operations we must check if the attribute controls a many-to-one association in order to show further
				// fields!
				if (assoc instanceof ManyToOneAssociation && methodType == DataExchangeMethodTypeEnumeration.IMPORT) {
					if (assoc.getTarget().getPKAttribute().equals(domainAttribute))
						isMappedToDomainAssociation = true;

					if (!isMappedToDomainAssociation && assoc.getTarget().getDisplayAttribute() != null
							&& assoc.getTarget().getDisplayAttribute().equals(domainAttribute))
						isMappedToDomainAssociation = true;
				}
			}
		}
	}

	/**
	 * @return the data exchange attribute
	 */
	public DataExchangeAttribute getDataExchangeAttribute() {
		return dataExchangeAttribute;
	}

	/**
	 * Save the data
	 */
	private void saveData() {
		if (!editMode) {
			dataExchangeAttribute = ExchangeFactory.eINSTANCE.createDataExchangeAttribute();
			dataExchangeAttribute.setElement(parentElement);
			parentElement.getAttributes().add(dataExchangeAttribute);

			// If the user enters a mapping attribute name we create an exchange mapping attribute!
			if (!txtMappedName.getText().isEmpty()) {
				final ExchangeMappingAttribute mappingAttribute = ExchangeFactory.eINSTANCE.createExchangeMappingAttribute();
				mappingAttribute.setMappingType(propType.getSelectedItem());
				mappingAttribute.setName(txtMappedName.getText());
				mappingAttribute.setExchangeMappingObject(parentElement.getMappingObject());
				mappingAttribute.setModifier(JavaTypeModifierEnumeration.NONE);

				parentElement.getMappingObject().getAttributes().add(mappingAttribute);
				dataExchangeAttribute.setMappingAttribute(mappingAttribute);
			}
		}

		dataExchangeAttribute.setName(txtName.getText());

		if (txtOrder.getText().isEmpty())
			dataExchangeAttribute.setAttributeOrder(null);
		else
			dataExchangeAttribute.setAttributeOrder(Integer.parseInt(txtOrder.getText()));

		dataExchangeAttribute.setOptional(chkOptional.getSelection());

		if (chkReadonly != null)
			dataExchangeAttribute.setReadonly(chkReadonly.getSelection());

		if (chkVisible != null)
			dataExchangeAttribute.setVisible(chkVisible.getSelection());

		if (dataExchangeAttribute.getMappingAttribute() != null) {
			if (txtMappedName != null)
				dataExchangeAttribute.getMappingAttribute().setName(txtMappedName.getText());

			if (chkInsertable != null)
				dataExchangeAttribute.getMappingAttribute().setInsertable(chkInsertable.getSelection());

			if (chkDisableExtMapping != null)
				dataExchangeAttribute.setDisableExternalMapping(chkDisableExtMapping.getSelection());

			if (chkCustomQuery != null)
				dataExchangeAttribute.setUsedForCustomQuery(chkCustomQuery.getSelection());

			if (chkUpdatable != null)
				dataExchangeAttribute.getMappingAttribute().setUpdatable(chkUpdatable.getSelection());

			if (txtSelectionListStatement != null)
				dataExchangeAttribute.getMappingAttribute().setSelectionListStatement(txtSelectionListStatement.getText());

			dataExchangeAttribute.getMappingAttribute().setDefaultValue(null);

			// A default value will be only saved if a mapping attribute exists!
			if (chkDefault != null && chkDefault.getSelection())
				dataExchangeAttribute.getMappingAttribute().setDefaultValue(DEFAULT_VALUE_FLAG);

			if (cboDefaultEnumLiteral != null && cboDefaultEnumLiteral.getSelectionIndex() > 0)
				dataExchangeAttribute.getMappingAttribute()
						.setDefaultValue(cboDefaultEnumLiteral.getItem(cboDefaultEnumLiteral.getSelectionIndex()));

			if (txtDefault != null)
				dataExchangeAttribute.getMappingAttribute().setDefaultValue(txtDefault.getText());
		}

		if (propType != null)
			dataExchangeAttribute.setDataType(propType.getSelectedItem());

		dataExchangeAttribute.getValueListEntries().clear();

		// The user is responsible to provide a valid format for this attribute!
		if (!txtFormat.getText().isEmpty())
			dataExchangeAttribute.setFormat(txtFormat.getText());
		else
			dataExchangeAttribute.setFormat(null);

		if (listValueListEntries != null)
			for (final String item : listValueListEntries.getItems()) {
				final ValueListEntry entry = ExchangeFactory.eINSTANCE.createValueListEntry();
				entry.setItemText(item);

				dataExchangeAttribute.getValueListEntries().add(entry);
			}
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean validateInput() {
		DataExchangeElement currentElement = null;

		if (txtName.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), title, "The name must not be empty!");
			txtName.setFocus();
			return false;
		}

		// Search for duplicate attribute names
		if (editMode)
			currentElement = dataExchangeAttribute.getElement();
		else
			currentElement = parentElement;

		for (final DataExchangeAttribute attr : currentElement.getAttributes()) {
			if (editMode && attr.equals(dataExchangeAttribute))
				continue;

			if (attr.getName().equals(txtName.getText())) {
				MessageDialog.openInformation(getShell(), title, "An attribute with the name '" + attr.getName() + "' already exists!");
				return false;
			}
		}

		if (!txtOrder.getText().isEmpty()) {
			try {
				Integer.parseInt(txtOrder.getText());
			}
			catch (final NumberFormatException e) {
				MessageDialog.openInformation(getShell(), title, "The attribute order requires an integer value!");
				txtOrder.setFocus();
				return false;
			}
		}

		if (txtMappedName != null && (editMode || !txtMappedName.getText().isEmpty())) {
			ExchangeMappingObject mappingObject = null;

			if (editMode)
				mappingObject = dataExchangeAttribute.getMappingAttribute().getExchangeMappingObject();
			else
				mappingObject = parentElement.getMappingObject();

			// Search for duplicate mapping attribute names
			for (final ExchangeMappingAttribute attr : mappingObject.getAttributes()) {
				if (editMode && attr.equals(dataExchangeAttribute.getMappingAttribute()))
					continue;

				if (attr.getName().equals(txtMappedName.getText())) {
					MessageDialog.openInformation(getShell(), title,
							"A mapping attribute with the name '" + attr.getName() + "' already exists!");
					return false;
				}
			}

			final IStatus status = EclipseIDEService.validateFieldName(txtMappedName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), title, status.getMessage());
				txtMappedName.setFocus();
				return false;
			}
		}

		if (propType != null) {
			final JavaType selType = propType.getSelectedItem();

			if (selType == null) {
				MessageDialog.openInformation(getShell(), title, "A mapping type must be selected!");
				propType.getControl().setFocus();
				return false;
			}
		}

		// Check the default value
		if (txtDefault != null && !txtDefault.getText().isEmpty()) {
			JavaType type = null;

			if (editMode) {
				if (dataExchangeAttribute.getMappingAttribute() != null)
					type = dataExchangeAttribute.getMappingAttribute().getJavaType();
			}
			else
				type = propType.getSelectedItem();

			if (type != null) {
				try {
					if (type.isInteger())
						Integer.parseInt(txtDefault.getText());
					else if (type.isLong())
						Long.parseLong(txtDefault.getText());
					else if (type.isDouble())
						Double.parseDouble(txtDefault.getText());
					else if (type.isFloat())
						Float.parseFloat(txtDefault.getText());
					else if (type.isBigDecimal())
						new BigDecimal(txtDefault.getText());
				}
				catch (final NumberFormatException e) {
					MessageDialog.openInformation(getShell(), title, "The default value is not valid!");
					txtDefault.setFocus();
					return false;
				}

				if (type.isChar() && txtDefault.getText().length() > 1) {
					MessageDialog.openInformation(getShell(), title, "The default value requires exactly one character!");
					txtDefault.setFocus();
					return false;
				}
			}
		}

		return true;
	}

	/**
	 * @param type
	 */
	private void setFieldForDefaultType(JavaType type) {
		if (chkDefault != null) {
			chkDefault.dispose();
			chkDefault = null;
		}

		if (txtDefault != null) {
			txtDefault.dispose();
			txtDefault = null;
		}

		if (cboDefaultEnumLiteral != null) {
			cboDefaultEnumLiteral.dispose();
			cboDefaultEnumLiteral = null;
		}

		if (type.isEnum()) {
			final var en = (JavaEnum) type;

			cboDefaultEnumLiteral = new Combo(panDialogArea, SWT.READ_ONLY);
			cboDefaultEnumLiteral.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
			cboDefaultEnumLiteral.add("");

			en.getEnumerationValues().forEach(value -> cboDefaultEnumLiteral.add(value.getName()));

			if (editMode && dataExchangeAttribute.getMappingAttribute() != null) {
				final String defaultValue = dataExchangeAttribute.getMappingAttribute().getDefaultValue();
				int selectionIndex = 0;

				if (defaultValue != null && !defaultValue.isEmpty()) {
					selectionIndex = cboDefaultEnumLiteral.indexOf(defaultValue);

					if (selectionIndex == -1)
						cboDefaultEnumLiteral.select(0);
					else
						cboDefaultEnumLiteral.select(selectionIndex);
				}
			}
		}
		else {
			if (type.isBoolean() || type.isTemporalType() || type.isUUID()) {
				chkDefault = new Button(panDialogArea, SWT.CHECK);
				chkDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
			}
			else {
				txtDefault = new Text(panDialogArea, SWT.BORDER);
				txtDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
			}

			if (editMode && dataExchangeAttribute.getMappingAttribute() != null) {
				final String defaultValue = dataExchangeAttribute.getMappingAttribute().getDefaultValue();

				if (defaultValue != null && !defaultValue.isEmpty()) {
					if (chkDefault != null && (type.isBoolean() || type.isTemporalType() || type.isUUID()))
						chkDefault.setSelection(true);
					else if (txtDefault != null)
						txtDefault.setText(defaultValue);
				}
			}
		}

		panDialogArea.layout();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblType = new Label(panDialogArea, SWT.NONE);
		lblType.setText("Mapping type:");

		if (editMode) {
			txtType = new Text(panDialogArea, SWT.BORDER);
			txtType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			txtType.setEnabled(false);
		}
		else {
			propType = new AbstractProposalTextField<>(panDialogArea, SWT.BORDER, MIN_FILTER_LENGTH) {
				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalData(java.lang.String)
				 */
				@Override
				public Collection<JavaType> getProposalData(String filter) {
					final var types = new BasicEList<JavaType>();

					for (final JavaType type : project.getAllSupportedTypes()) {
						if (type.isByteArray())
							continue;

						if (type.isMappable() && type.getName().toLowerCase().startsWith(filter.toLowerCase()))
							types.add(type);
					}

					ECollections.sort(types, new JavaTypeComparator());

					return types;
				}

				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#onProposalAccepted(java.lang.Object)
				 */
				@Override
				public void onProposalAccepted(JavaType type) {
					setFieldForDefaultType(type);
				}

				/*
				 * (non-Javadoc)
				 * @see net.codecadenza.runtime.richclient.eclipse.widget.AbstractProposalTextField#getProposalLabel(java.lang.Object)
				 */
				@Override
				public String getProposalLabel(JavaType element) {
					return element.getName();
				}
			};

			propType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}

		final var lblAttributeOrder = new Label(panDialogArea, SWT.NONE);
		lblAttributeOrder.setText("Attribute order:");

		txtOrder = new Text(panDialogArea, SWT.BORDER);
		txtOrder.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblOptional = new Label(panDialogArea, SWT.NONE);
		lblOptional.setText("Optional:");

		chkOptional = new Button(panDialogArea, SWT.CHECK);

		if (!editMode)
			chkOptional.setSelection(true);

		if (contentType == ContentTypeEnumeration.EXCEL97 || contentType == ContentTypeEnumeration.EXCEL2007) {
			final var lblVisible = new Label(panDialogArea, SWT.NONE);
			lblVisible.setText("Visible:");

			chkVisible = new Button(panDialogArea, SWT.CHECK);
			chkVisible.setSelection(true);

			final var lblReadonly = new Label(panDialogArea, SWT.NONE);
			lblReadonly.setText("Readonly:");

			chkReadonly = new Button(panDialogArea, SWT.CHECK);
			chkReadonly.setSelection(false);
		}

		if (isMapped || !editMode) {
			final var lblMappingName = new Label(panDialogArea, SWT.NONE);
			lblMappingName.setText("Mapping attribute name:");

			txtMappedName = new Text(panDialogArea, SWT.BORDER);
			txtMappedName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			final var lblFormat = new Label(panDialogArea, SWT.NONE);
			lblFormat.setText("Format:");

			txtFormat = new Text(panDialogArea, SWT.BORDER);
			txtFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}
		else {
			final var lblFormat = new Label(panDialogArea, SWT.NONE);
			lblFormat.setText("Format:");

			txtFormat = new Text(panDialogArea, SWT.BORDER);
			txtFormat.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
		}

		if ((isMappedToDomainAssociation || isMappedToDomainAttribute) && methodType == DataExchangeMethodTypeEnumeration.IMPORT) {
			final var lblInsertable = new Label(panDialogArea, SWT.NONE);
			lblInsertable.setText("Insertable:");

			chkInsertable = new Button(panDialogArea, SWT.CHECK);

			final var lblUpdatable = new Label(panDialogArea, SWT.NONE);
			lblUpdatable.setText("Updatable:");

			chkUpdatable = new Button(panDialogArea, SWT.CHECK);
		}

		if (contentType == ContentTypeEnumeration.EXCEL97 || contentType == ContentTypeEnumeration.EXCEL2007) {
			final var lblSelectionListStatement = new Label(panDialogArea, SWT.NONE);
			lblSelectionListStatement.setText("Selection list statement:");

			txtSelectionListStatement = new Text(panDialogArea, SWT.BORDER);
			txtSelectionListStatement.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
			txtSelectionListStatement.setToolTipText("Enter a valid query statement, e.g. select a.name from Object a order by a.name");
		}

		if (!mappedToElementCollection) {
			final var lblValueListEntries = new Label(panDialogArea, SWT.NONE);
			lblValueListEntries.setLayoutData(new GridData(SWT.LEFT, SWT.TOP, false, false));
			lblValueListEntries.setText("Value list entries:");

			listValueListEntries = new List(panDialogArea, SWT.BORDER);
			listValueListEntries.setLayoutData(new GridData(SWT.FILL, SWT.FILL, false, true, 3, 1));

			final var menu = new Menu(listValueListEntries);
			listValueListEntries.setMenu(menu);

			final var mnuAdd = new MenuItem(menu, SWT.NONE);
			mnuAdd.setText("Add");

			mnuAdd.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					final var msgTitle = "Create new entry";
					final var dlg = new InputDialog(getShell(), msgTitle, "Enter the name of a new list item:", "", null);

					if (dlg.open() == Window.OK && !dlg.getValue().isEmpty())
						listValueListEntries.add(dlg.getValue());
				}
			});

			final var mnuRemove = new MenuItem(menu, SWT.NONE);
			mnuRemove.setText("Remove");

			mnuRemove.addSelectionListener(new SelectionAdapter() {
				/*
				 * (non-Javadoc)
				 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
				 */
				@Override
				public void widgetSelected(SelectionEvent e) {
					listValueListEntries.remove(listValueListEntries.getSelectionIndex());
				}
			});
		}

		if (!editMode && !mappedToElementCollection) {
			final var lblDefaultValue = new Label(panDialogArea, SWT.NONE);
			lblDefaultValue.setText("Default value:");

			txtDefault = new Text(panDialogArea, SWT.BORDER);
			txtDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
		}

		if (isMapped && methodType == DataExchangeMethodTypeEnumeration.IMPORT) {
			final var glDisableExtMapping = new GridLayout(2, false);
			glDisableExtMapping.marginWidth = 0;
			glDisableExtMapping.marginHeight = 0;

			final var panDisableExtMapping = new Composite(panDialogArea, SWT.NONE);
			panDisableExtMapping.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 4, 1));
			panDisableExtMapping.setLayout(glDisableExtMapping);

			final var lblDisableExtMapping = new Label(panDisableExtMapping, SWT.NONE);
			lblDisableExtMapping.setText("Disable mapping to external data structure:");

			chkDisableExtMapping = new Button(panDisableExtMapping, SWT.CHECK);

			final var glCustomQuery = new GridLayout(2, false);
			glCustomQuery.marginWidth = 0;
			glCustomQuery.marginHeight = 0;

			if (!mappedToElementCollection) {
				final var panCustomQuery = new Composite(panDialogArea, SWT.NONE);
				panCustomQuery.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 4, 1));
				panCustomQuery.setLayout(glCustomQuery);

				final var lblCustomQuery = new Label(panCustomQuery, SWT.NONE);
				lblCustomQuery.setText("Use for custom queries to search for root domain objects:");

				chkCustomQuery = new Button(panCustomQuery, SWT.CHECK);
			}
		}

		if (editMode) {
			txtName.setText(dataExchangeAttribute.getName());

			if (dataExchangeAttribute.getAttributeOrder() != null)
				txtOrder.setText(Integer.toString(dataExchangeAttribute.getAttributeOrder()));

			chkOptional.setSelection(dataExchangeAttribute.isOptional());

			if (chkReadonly != null)
				chkReadonly.setSelection(dataExchangeAttribute.isReadonly());

			if (chkVisible != null)
				chkVisible.setSelection(dataExchangeAttribute.isVisible());

			if (dataExchangeAttribute.getFormat() != null)
				txtFormat.setText(dataExchangeAttribute.getFormat());

			if (isMapped) {
				final ExchangeMappingAttribute mappingAttribute = dataExchangeAttribute.getMappingAttribute();

				if (txtMappedName != null)
					txtMappedName.setText(mappingAttribute.getName());

				if (txtType != null)
					txtType.setText(mappingAttribute.getJavaType().getName());

				if (chkInsertable != null)
					chkInsertable.setSelection(mappingAttribute.isInsertable());

				if (chkDisableExtMapping != null)
					chkDisableExtMapping.setSelection(dataExchangeAttribute.isDisableExternalMapping());

				if (chkCustomQuery != null)
					chkCustomQuery.setSelection(dataExchangeAttribute.isUsedForCustomQuery());

				if (chkUpdatable != null)
					chkUpdatable.setSelection(mappingAttribute.isUpdatable());

				if (txtSelectionListStatement != null && mappingAttribute.getSelectionListStatement() != null)
					txtSelectionListStatement.setText(mappingAttribute.getSelectionListStatement());

				if (!mappedToElementCollection) {
					final var lblDefaultValue = new Label(panDialogArea, SWT.NONE);
					lblDefaultValue.setText("Default value:");

					setFieldForDefaultType(mappingAttribute.getJavaType());
				}
			}
			else if (txtType != null)
				txtType.setText(dataExchangeAttribute.getDataType().getName());

			dataExchangeAttribute.getValueListEntries().forEach(entry -> listValueListEntries.add(entry.getItemText()));
		}

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(Math.max(600, super.getInitialSize().x), super.getInitialSize().y);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)
	 */
	@Override
	protected void buttonPressed(int buttonId) {
		if (buttonId == IDialogConstants.OK_ID) {
			if (!validateInput())
				return;

			saveData();
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

		if (editMode)
			newShell.setText("Edit data exchange attribute '" + dataExchangeAttribute.getName() + "'");
		else
			newShell.setText("Add new data exchange attribute to element '" + parentElement.getName() + "'");
	}

}
