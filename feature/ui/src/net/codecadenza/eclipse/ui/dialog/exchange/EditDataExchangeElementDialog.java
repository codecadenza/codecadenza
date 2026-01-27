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

import java.math.BigDecimal;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.core.runtime.IStatus;
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
 * Dialog for maintaining data exchange elements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDataExchangeElementDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Edit data exchange element";

	private Text txtName;
	private Text txtMinOccurrences;
	private Text txtMaxOccurrences;
	private Text txtElementOrder;
	private Text txtSchemaTypeName;
	private Text txtWrapperElementName;
	private Text txtType;
	private Text txtMappingAttrName;
	private Button chkUpdatable;
	private Button chkDeleteAllItems;
	private Button chkAddNewItems;
	private Button chkUpdateExistingItems;
	private Text txtDefault;
	private Button chkDefault;
	private Combo cboDefaultEnumLiteral;
	private DataExchangeElement dataExchangeElement;
	private ExchangeMappingAttribute mappingAttribute;
	private Button chkInsertable;
	private Button chkDisableExtMapping;
	private Button chkCustomQuery;
	private boolean mappedToDomainAttribute;
	private boolean mappedToMTOContainer;
	private boolean mappedToOTOContainer;
	private boolean mappedToOTMContainer;
	private boolean mappedToAssociationController;
	private boolean rootElement;
	private ContentTypeEnumeration contentType;
	private DataExchangeMethodTypeEnumeration methodType;
	private List listValueListEntries;
	private Text txtSelListStatement;
	private boolean processSingleObject;
	private boolean mappedToElementCollection;

	/**
	 * Constructor
	 * @param parentShell
	 * @param element
	 * @param rootElement
	 * @param contentType
	 * @param methodType
	 * @param processSingleObject
	 */
	public EditDataExchangeElementDialog(Shell parentShell, DataExchangeElement element, boolean rootElement,
			ContentTypeEnumeration contentType, DataExchangeMethodTypeEnumeration methodType, boolean processSingleObject) {
		super(parentShell);

		this.dataExchangeElement = element;
		this.mappingAttribute = element.getMappingAttribute();
		this.contentType = contentType;
		this.methodType = methodType;
		this.rootElement = rootElement;
		this.processSingleObject = processSingleObject;

		if (mappingAttribute == null)
			return;

		if (mappingAttribute.getDomainAttribute() != null) {
			mappedToDomainAttribute = true;
			mappedToElementCollection = mappingAttribute.getDomainAttribute().getCollectionType() != CollectionTypeEnumeration.NONE;

			final DomainAttribute domainAttribute = mappingAttribute.getDomainAttribute();
			final AbstractDomainAssociation assoc = mappingAttribute.getAssociation();
			final ExchangeMappingAttribute parentMappingAttr = element.getParentElement().getMappingAttribute();

			// In case of import operations we must check if the attribute controls a many-to-one association in order to show further
			// fields!
			if (assoc instanceof ManyToOneAssociation && methodType == DataExchangeMethodTypeEnumeration.IMPORT
					&& (parentMappingAttr == null || parentMappingAttr.getAssociation() == null
							|| !parentMappingAttr.getAssociation().equals(assoc))) {
				if (assoc.getTarget().getPKAttribute().equals(domainAttribute))
					mappedToAssociationController = true;

				if (!mappedToAssociationController && assoc.getTarget().getDisplayAttribute() != null
						&& assoc.getTarget().getDisplayAttribute().equals(domainAttribute))
					mappedToAssociationController = true;
			}
		}
		else if (mappingAttribute.getAssociation() != null) {
			if (mappingAttribute.getAssociation() instanceof OneToManyAssociation)
				mappedToOTMContainer = true;
			else if (mappingAttribute.getAssociation() instanceof ManyToOneAssociation)
				mappedToMTOContainer = true;
			else if (mappingAttribute.getAssociation() instanceof OneToOneAssociation)
				mappedToOTOContainer = true;
		}
	}

	/**
	 * Save the data
	 */
	private void saveData() {
		dataExchangeElement.setName(txtName.getText());

		if (dataExchangeElement.isContainer())
			dataExchangeElement.getMappingObject().setName(txtType.getText());

		if (txtMinOccurrences != null) {
			if (txtMinOccurrences.getText().isEmpty())
				dataExchangeElement.setMinOccurrences(null);
			else
				dataExchangeElement.setMinOccurrences(Integer.parseInt(txtMinOccurrences.getText()));
		}

		if (txtMaxOccurrences != null) {
			if (txtMaxOccurrences.getText().isEmpty())
				dataExchangeElement.setMaxOccurrences(null);
			else
				dataExchangeElement.setMaxOccurrences(Integer.parseInt(txtMaxOccurrences.getText()));
		}

		if (txtElementOrder != null) {
			if (txtElementOrder.getText().isEmpty())
				dataExchangeElement.setElementOrder(null);
			else
				dataExchangeElement.setElementOrder(Integer.parseInt(txtElementOrder.getText()));
		}

		if (txtSchemaTypeName != null) {
			if (txtSchemaTypeName.getText().isEmpty())
				dataExchangeElement.setTypeName(null);
			else
				dataExchangeElement.setTypeName(txtSchemaTypeName.getText());
		}

		if (txtWrapperElementName != null) {
			if (txtWrapperElementName.getText().isEmpty())
				dataExchangeElement.setWrapperElementName(null);
			else
				dataExchangeElement.setWrapperElementName(txtWrapperElementName.getText());
		}

		if (rootElement) {
			if (chkAddNewItems != null)
				dataExchangeElement.getMappingObject().setAddNewItems(chkAddNewItems.getSelection());

			if (chkUpdateExistingItems != null)
				dataExchangeElement.getMappingObject().setUpdateExistingItems(chkUpdateExistingItems.getSelection());

			if (chkDeleteAllItems != null)
				dataExchangeElement.getMappingObject().setDeleteAllItems(chkDeleteAllItems.getSelection());
		}

		if (listValueListEntries != null) {
			dataExchangeElement.getValueListEntries().clear();

			for (final String item : listValueListEntries.getItems()) {
				final ValueListEntry entry = ExchangeFactory.eINSTANCE.createValueListEntry();
				entry.setItemText(item);

				dataExchangeElement.getValueListEntries().add(entry);
			}
		}

		if (mappingAttribute != null) {
			if (txtMappingAttrName != null)
				mappingAttribute.setName(txtMappingAttrName.getText());

			if (chkInsertable != null)
				mappingAttribute.setInsertable(chkInsertable.getSelection());

			if (chkUpdatable != null)
				mappingAttribute.setUpdatable(chkUpdatable.getSelection());

			if (chkDisableExtMapping != null)
				dataExchangeElement.setDisableExternalMapping(chkDisableExtMapping.getSelection());

			if (chkCustomQuery != null)
				dataExchangeElement.setUsedForCustomQuery(chkCustomQuery.getSelection());

			if (!rootElement) {
				if (chkAddNewItems != null)
					mappingAttribute.setAddNewItems(chkAddNewItems.getSelection());

				if (chkUpdateExistingItems != null)
					mappingAttribute.setUpdateExistingItems(chkUpdateExistingItems.getSelection());

				if (chkDeleteAllItems != null)
					mappingAttribute.setDeleteAllItems(chkDeleteAllItems.getSelection());
			}

			if (txtSelListStatement != null) {
				if (txtSelListStatement.getText().isEmpty())
					mappingAttribute.setSelectionListStatement(null);
				else
					mappingAttribute.setSelectionListStatement(txtSelListStatement.getText());
			}

			mappingAttribute.setDefaultValue(null);

			// A default value will be only saved if a mapping attribute exists!
			if (chkDefault != null && chkDefault.getSelection())
				mappingAttribute.setDefaultValue(DEFAULT_VALUE_FLAG);

			if (cboDefaultEnumLiteral != null && cboDefaultEnumLiteral.getSelectionIndex() > 0)
				mappingAttribute.setDefaultValue(cboDefaultEnumLiteral.getItem(cboDefaultEnumLiteral.getSelectionIndex()));

			if (txtDefault != null)
				mappingAttribute.setDefaultValue(txtDefault.getText());
		}
	}

	/**
	 * Validate the user input
	 * @return true if the validation was successful
	 */
	private boolean validateInput() {
		Integer minOccurrences = null;
		Integer maxOccurrences = null;

		if (txtName.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The element name must not be empty!");
			txtName.setFocus();
			return false;
		}

		if (txtMinOccurrences != null && !txtMinOccurrences.getText().isEmpty()) {
			try {
				minOccurrences = Integer.parseInt(txtMinOccurrences.getText());
			}
			catch (final NumberFormatException _) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The lower bound requires an integer value!");
				txtMinOccurrences.setFocus();
				return false;
			}

			if (minOccurrences < 0) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The lower bound requires a positive integer value!");
				txtMinOccurrences.setFocus();
				return false;
			}
		}

		if (txtMaxOccurrences != null && !txtMaxOccurrences.getText().isEmpty()) {
			try {
				maxOccurrences = Integer.parseInt(txtMaxOccurrences.getText());
			}
			catch (final NumberFormatException _) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The upper bound requires an integer value!");
				txtMaxOccurrences.setFocus();
				return false;
			}

			if (maxOccurrences < 1) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The upper bound must be undefined or greater than 1!");
				txtMaxOccurrences.setFocus();
				return false;
			}
		}

		if (mappedToDomainAttribute && minOccurrences != null && (minOccurrences > 1 || minOccurrences < 0)) {
			final var msg = "The lower bound for elements that are mapped to a domain attribute must be 0, 1 or undefined!";

			MessageDialog.openInformation(getShell(), DLG_TITLE, msg);
			txtMinOccurrences.setFocus();
			return false;
		}

		if (minOccurrences != null && maxOccurrences != null && minOccurrences > maxOccurrences) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The lower bound must be less or equal than the upper bound!");
			txtMinOccurrences.setFocus();
			return false;
		}

		if (txtElementOrder != null && !txtElementOrder.getText().isEmpty()) {
			try {
				Integer.parseInt(txtElementOrder.getText());
			}
			catch (final NumberFormatException _) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The element order requires an integer value!");
				txtElementOrder.setFocus();
				return false;
			}
		}

		if (txtSchemaTypeName != null && txtSchemaTypeName.getText().isEmpty()) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "The schema type name must not be empty!");
			txtSchemaTypeName.setFocus();
			return false;
		}

		if (txtMappingAttrName != null) {
			final IStatus status = EclipseIDEService.validateFieldName(txtMappingAttrName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				txtMappingAttrName.setFocus();
				return false;
			}
		}

		if (dataExchangeElement.isContainer()) {
			final IStatus status = EclipseIDEService.validateJavaTypeName(txtType.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				txtType.setFocus();
				return false;
			}
		}

		// Check the default value
		if (txtDefault != null && !txtDefault.getText().isEmpty() && mappingAttribute != null && !dataExchangeElement.isContainer()) {
			final JavaType type = mappingAttribute.getJavaType();

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
			catch (final NumberFormatException _) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The default value is not valid!");
				txtDefault.setFocus();
				return false;
			}

			if (type.isChar() && txtDefault.getText().length() > 1) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The default value requires exactly one character!");
				txtDefault.setFocus();
				return false;
			}
		}

		return true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblType = new Label(panDialogArea, SWT.NONE);
		lblType.setText("Mapping type:");

		txtType = new Text(panDialogArea, SWT.BORDER);
		txtType.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtType.setEnabled(false);

		if (dataExchangeElement.isContainer())
			txtType.setEnabled(true);

		final var lblMinOccurrences = new Label(panDialogArea, SWT.NONE);
		lblMinOccurrences.setText("Min. occurrences:");

		txtMinOccurrences = new Text(panDialogArea, SWT.BORDER);
		txtMinOccurrences.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (rootElement)
			txtMinOccurrences.setEditable(false);

		final var lblMaxOccurences = new Label(panDialogArea, SWT.NONE);
		lblMaxOccurences.setText("Max. occurrences:");

		txtMaxOccurrences = new Text(panDialogArea, SWT.BORDER);
		txtMaxOccurrences.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (rootElement || !mappedToElementCollection)
			txtMaxOccurrences.setEditable(false);

		final var lblElementOrder = new Label(panDialogArea, SWT.NONE);
		lblElementOrder.setText("Element order:");

		txtElementOrder = new Text(panDialogArea, SWT.BORDER);
		txtElementOrder.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		new Label(panDialogArea, SWT.NONE);
		new Label(panDialogArea, SWT.NONE);

		if (dataExchangeElement.isContainer() && contentType == ContentTypeEnumeration.XML) {
			final var lblTypeName = new Label(panDialogArea, SWT.NONE);
			lblTypeName.setText("Schema type name:");

			txtSchemaTypeName = new Text(panDialogArea, SWT.BORDER);
			txtSchemaTypeName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			// A wrapper element makes no sense for root elements!
			if (!rootElement) {
				final var lblWrapperElement = new Label(panDialogArea, SWT.NONE);
				lblWrapperElement.setText("Wrapper element name:");

				txtWrapperElementName = new Text(panDialogArea, SWT.BORDER);
				txtWrapperElementName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
			}
			else {
				new Label(panDialogArea, SWT.NONE);
				new Label(panDialogArea, SWT.NONE);
			}
		}

		// For root elements it must be possible to maintain important information that control generator output!
		if (methodType == DataExchangeMethodTypeEnumeration.IMPORT && rootElement) {
			final var lblAddNewItems = new Label(panDialogArea, SWT.NONE);

			if (processSingleObject)
				lblAddNewItems.setText("Create new item:");
			else
				lblAddNewItems.setText("Create new items:");

			chkAddNewItems = new Button(panDialogArea, SWT.CHECK);

			final var lbUpdateExistingItems = new Label(panDialogArea, SWT.NONE);

			if (processSingleObject)
				lbUpdateExistingItems.setText("Update existing item:");
			else
				lbUpdateExistingItems.setText("Update existing items:");

			chkUpdateExistingItems = new Button(panDialogArea, SWT.CHECK);

			final var lblDeleteAllItems = new Label(panDialogArea, SWT.NONE);

			if (processSingleObject)
				lblDeleteAllItems.setText("Delete existing item:");
			else
				lblDeleteAllItems.setText("Delete all existing items:");

			chkDeleteAllItems = new Button(panDialogArea, SWT.CHECK);

			new Label(panDialogArea, SWT.NONE);
			new Label(panDialogArea, SWT.NONE);
		}

		if (mappingAttribute != null) {
			final var lblMapAttrName = new Label(panDialogArea, SWT.NONE);
			lblMapAttrName.setText("Mapping attribute name:");

			txtMappingAttrName = new Text(panDialogArea, SWT.BORDER);
			txtMappingAttrName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

			new Label(panDialogArea, SWT.NONE);
			new Label(panDialogArea, SWT.NONE);

			if (methodType == DataExchangeMethodTypeEnumeration.IMPORT) {
				if (mappedToDomainAttribute || mappedToMTOContainer || mappedToOTOContainer) {
					final var lblInsertable = new Label(panDialogArea, SWT.NONE);
					lblInsertable.setText("Insertable:");

					chkInsertable = new Button(panDialogArea, SWT.CHECK);

					final var lblUpdatable = new Label(panDialogArea, SWT.NONE);
					lblUpdatable.setText("Updatable:");

					chkUpdatable = new Button(panDialogArea, SWT.CHECK);
				}

				if (mappedToOTMContainer) {
					final var lblAddNewItems = new Label(panDialogArea, SWT.NONE);
					lblAddNewItems.setText("Add new items:");

					chkAddNewItems = new Button(panDialogArea, SWT.CHECK);

					final var lblUpdateExistingItems = new Label(panDialogArea, SWT.NONE);
					lblUpdateExistingItems.setText("Update existing items:");

					chkUpdateExistingItems = new Button(panDialogArea, SWT.CHECK);

					final var lblDeleteAllItems = new Label(panDialogArea, SWT.NONE);
					lblDeleteAllItems.setText("Delete all items:");

					chkDeleteAllItems = new Button(panDialogArea, SWT.CHECK);

					new Label(panDialogArea, SWT.NONE);
					new Label(panDialogArea, SWT.NONE);
				}

				if (mappedToAssociationController || mappedToMTOContainer || mappedToOTOContainer) {
					final var lblSelListStatement = new Label(panDialogArea, SWT.NONE);
					lblSelListStatement.setText("Selection list statement:");

					txtSelListStatement = new Text(panDialogArea, SWT.BORDER);
					txtSelListStatement.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
				}
			}
		}

		if (!dataExchangeElement.isContainer() && !mappedToElementCollection) {
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
					final int selectionIndex = listValueListEntries.getSelectionIndex();

					if (selectionIndex == -1)
						return;

					listValueListEntries.remove(selectionIndex);
				}
			});
		}

		if (mappedToDomainAttribute && methodType == DataExchangeMethodTypeEnumeration.IMPORT) {
			final var glDisableExtMapping = new GridLayout(2, false);
			glDisableExtMapping.marginWidth = 0;
			glDisableExtMapping.marginHeight = 0;

			final var panDisableExtMapping = new Composite(panDialogArea, SWT.NONE);
			panDisableExtMapping.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 4, 1));
			panDisableExtMapping.setLayout(glDisableExtMapping);

			final var lblDisableExtMapping = new Label(panDisableExtMapping, SWT.NONE);
			lblDisableExtMapping.setText("Disable mapping to external data structure:");

			chkDisableExtMapping = new Button(panDisableExtMapping, SWT.CHECK);

			if (!mappedToElementCollection) {
				final var glCustomQuery = new GridLayout(2, false);
				glCustomQuery.marginWidth = 0;
				glCustomQuery.marginHeight = 0;

				final var panCustomQuery = new Composite(panDialogArea, SWT.NONE);
				panCustomQuery.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 4, 1));
				panCustomQuery.setLayout(glCustomQuery);

				final var lblCustomQuery = new Label(panCustomQuery, SWT.NONE);
				lblCustomQuery.setText("Use for custom queries to search for root domain objects:");

				chkCustomQuery = new Button(panCustomQuery, SWT.CHECK);
			}
		}

		txtName.setText(dataExchangeElement.getName());

		if (dataExchangeElement.getMinOccurrences() != null)
			txtMinOccurrences.setText(dataExchangeElement.getMinOccurrences().toString());

		if (dataExchangeElement.getMaxOccurrences() != null)
			txtMaxOccurrences.setText(dataExchangeElement.getMaxOccurrences().toString());

		if (dataExchangeElement.getElementOrder() != null)
			txtElementOrder.setText(dataExchangeElement.getElementOrder().toString());

		if (txtSchemaTypeName != null && !mappedToDomainAttribute && dataExchangeElement.getTypeName() != null)
			txtSchemaTypeName.setText(dataExchangeElement.getTypeName());

		if (txtWrapperElementName != null && dataExchangeElement.getWrapperElementName() != null)
			txtWrapperElementName.setText(dataExchangeElement.getWrapperElementName());

		if (txtMappingAttrName != null)
			txtMappingAttrName.setText(mappingAttribute.getName());

		if (chkInsertable != null)
			chkInsertable.setSelection(mappingAttribute.isInsertable());

		if (chkDisableExtMapping != null)
			chkDisableExtMapping.setSelection(dataExchangeElement.isDisableExternalMapping());

		if (chkCustomQuery != null)
			chkCustomQuery.setSelection(dataExchangeElement.isUsedForCustomQuery());

		if (chkUpdatable != null)
			chkUpdatable.setSelection(mappingAttribute.isUpdatable());

		if (rootElement) {
			if (chkAddNewItems != null)
				chkAddNewItems.setSelection(dataExchangeElement.getMappingObject().isAddNewItems());

			if (chkUpdateExistingItems != null)
				chkUpdateExistingItems.setSelection(dataExchangeElement.getMappingObject().isUpdateExistingItems());

			if (chkDeleteAllItems != null)
				chkDeleteAllItems.setSelection(dataExchangeElement.getMappingObject().isDeleteAllItems());
		}
		else {
			if (chkAddNewItems != null)
				chkAddNewItems.setSelection(mappingAttribute.isAddNewItems());

			if (chkUpdateExistingItems != null)
				chkUpdateExistingItems.setSelection(mappingAttribute.isUpdateExistingItems());

			if (chkDeleteAllItems != null)
				chkDeleteAllItems.setSelection(mappingAttribute.isDeleteAllItems());
		}

		if (txtSelListStatement != null && mappingAttribute.getSelectionListStatement() != null)
			txtSelListStatement.setText(mappingAttribute.getSelectionListStatement());

		if (dataExchangeElement.isContainer())
			txtType.setText(dataExchangeElement.getMappingObject().getName());
		else if (mappingAttribute != null)
			txtType.setText(mappingAttribute.getJavaType().getName());
		else
			txtType.setText(dataExchangeElement.getDataType().getName());

		if (listValueListEntries != null)
			dataExchangeElement.getValueListEntries().forEach(entry -> listValueListEntries.add(entry.getItemText()));

		if (mappingAttribute != null && !dataExchangeElement.isContainer() && !mappedToElementCollection) {
			final var lblDefaultValue = new Label(panDialogArea, SWT.NONE);
			lblDefaultValue.setText("Default value:");

			final JavaType type = mappingAttribute.getJavaType();

			if (type.isEnum()) {
				final var en = (JavaEnum) type;

				cboDefaultEnumLiteral = new Combo(panDialogArea, SWT.READ_ONLY);
				cboDefaultEnumLiteral.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
				cboDefaultEnumLiteral.add("");

				en.getEnumerationValues().forEach(value -> cboDefaultEnumLiteral.add(value.getName()));

				final String defaultValue = mappingAttribute.getDefaultValue();
				int selectionIndex = 0;

				if (defaultValue != null && !defaultValue.isEmpty()) {
					selectionIndex = cboDefaultEnumLiteral.indexOf(defaultValue);

					if (selectionIndex == -1)
						cboDefaultEnumLiteral.select(0);
					else
						cboDefaultEnumLiteral.select(selectionIndex);
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

				final String defaultValue = mappingAttribute.getDefaultValue();

				if (defaultValue != null) {
					if (type.isBoolean() || type.isTemporalType() || type.isUUID())
						chkDefault.setSelection(!defaultValue.isEmpty());
					else
						txtDefault.setText(defaultValue);
				}
			}
		}

		return panDialogArea;
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
	 * @see org.eclipse.jface.dialogs.Dialog#getInitialSize()
	 */
	@Override
	protected Point getInitialSize() {
		return new Point(Math.max(600, super.getInitialSize().x), super.getInitialSize().y);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		newShell.setText("Edit data exchange element '" + dataExchangeElement.getName() + "'");
	}

}
