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
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ValueListEntry;
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
import org.eclipse.swt.widgets.Button;
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
 * Dialog for creating new data exchange elements
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CreateNewDataExchangeElementDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Create new data exchange element";

	public enum Mode {
		UNMAPPED, MAPPED
	}

	private Text txtName;
	private Text txtMinOccurrences;
	private Text txtMaxOccurrences;
	private Text txtElementOrder;
	private Text txtMappingAttrName;
	private final DataExchangeElement parentElement;
	private AbstractProposalTextField<JavaType> propType;
	private List listValueListEntries;
	private final Mode mode;
	private final Project project;
	private DataExchangeElement dataExchangeElement;
	private Text txtDefault;
	private Button chkDefault;
	private Composite panDialogArea;

	/**
	 * Constructor
	 * @param parentShell
	 * @param parentElement
	 * @param project
	 * @param mode
	 */
	public CreateNewDataExchangeElementDialog(Shell parentShell, DataExchangeElement parentElement, Project project, Mode mode) {
		super(parentShell);

		this.parentElement = parentElement;
		this.mode = mode;
		this.project = project;
	}

	/**
	 * @return the new data exchange element
	 */
	public DataExchangeElement getDataExchangeElement() {
		return dataExchangeElement;
	}

	/**
	 * Save the data
	 */
	private void saveData() {
		dataExchangeElement = ExchangeFactory.eINSTANCE.createDataExchangeElement();
		dataExchangeElement.setName(txtName.getText());

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

		if (mode == Mode.MAPPED) {
			final ExchangeMappingAttribute mappingAttribute = ExchangeFactory.eINSTANCE.createExchangeMappingAttribute();
			mappingAttribute.setName(txtMappingAttrName.getText());
			mappingAttribute.setMappingType(propType.getSelectedItem());
			mappingAttribute.setModifier(JavaTypeModifierEnumeration.NONE);

			dataExchangeElement.setMappingAttribute(mappingAttribute);
			mappingAttribute.setExchangeMappingObject(parentElement.getMappingObject());
			parentElement.getMappingObject().getAttributes().add(mappingAttribute);

			// A default value will be only saved if a mapping attribute exists!
			if (chkDefault != null && chkDefault.getSelection())
				mappingAttribute.setDefaultValue(DEFAULT_VALUE_FLAG);

			if (txtDefault != null)
				mappingAttribute.setDefaultValue(txtDefault.getText());
		}
		else
			dataExchangeElement.setDataType(propType.getSelectedItem());

		dataExchangeElement.getValueListEntries().clear();

		for (final String item : listValueListEntries.getItems()) {
			final ValueListEntry entry = ExchangeFactory.eINSTANCE.createValueListEntry();
			entry.setItemText(item);

			dataExchangeElement.getValueListEntries().add(entry);
		}

		parentElement.getSubElements().add(dataExchangeElement);
		dataExchangeElement.setParentElement(parentElement);
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

		if (type.isBoolean() || type.isTemporalType() || type.isUUID()) {
			chkDefault = new Button(panDialogArea, SWT.CHECK);
			chkDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
		}
		else {
			txtDefault = new Text(panDialogArea, SWT.BORDER);
			txtDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
		}

		panDialogArea.layout();
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

		// Search for duplicate element names
		for (final DataExchangeElement element : parentElement.getSubElements()) {
			if (element.getName().equals(txtName.getText())) {
				MessageDialog.openInformation(getShell(), DLG_TITLE,
						"An element with the name '" + element.getName() + "' already exists!");
				return false;
			}
		}

		if (propType.getSelectedItem() == null) {
			MessageDialog.openInformation(getShell(), DLG_TITLE, "A mapping type must be selected!");
			propType.getControl().setFocus();
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
				MessageDialog.openInformation(getShell(), DLG_TITLE, "The lower bound requires a positive integer value");
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

		if (txtMappingAttrName != null) {
			// Search for duplicate mapping attribute names
			for (final ExchangeMappingAttribute attr : parentElement.getMappingObject().getAttributes()) {
				if (attr.getName().equals(txtMappingAttrName.getText())) {
					MessageDialog.openInformation(getShell(), DLG_TITLE,
							"An attribute with the name '" + attr.getName() + "' already exists!");
					return false;
				}
			}

			final IStatus status = EclipseIDEService.validateFieldName(txtMappingAttrName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), DLG_TITLE, status.getMessage());
				txtMappingAttrName.setFocus();
				return false;
			}

			// Check the default value
			if (txtDefault != null && !txtDefault.getText().isEmpty()) {
				final JavaType type = propType.getSelectedItem();

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
		panDialogArea = (Composite) super.createDialogArea(parent, 4);

		final var lblName = new Label(panDialogArea, SWT.NONE);
		lblName.setText("Name:");

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblType = new Label(panDialogArea, SWT.NONE);
		lblType.setText("Mapping type:");

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
				if (mode == Mode.MAPPED)
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

		final var lblMinOccurrences = new Label(panDialogArea, SWT.NONE);
		lblMinOccurrences.setText("Min. occurrences:");

		txtMinOccurrences = new Text(panDialogArea, SWT.BORDER);
		txtMinOccurrences.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblMaxOccurences = new Label(panDialogArea, SWT.NONE);
		lblMaxOccurences.setText("Max. occurrences:");

		txtMaxOccurrences = new Text(panDialogArea, SWT.BORDER);
		txtMaxOccurrences.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		txtMaxOccurrences.setText("1");

		final var lblElementOrder = new Label(panDialogArea, SWT.NONE);
		lblElementOrder.setText("Element order:");

		txtElementOrder = new Text(panDialogArea, SWT.BORDER);
		txtElementOrder.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		if (mode == Mode.MAPPED) {
			final var lblMapAttrName = new Label(panDialogArea, SWT.NONE);
			lblMapAttrName.setText("Mapping attribute name:");

			txtMappingAttrName = new Text(panDialogArea, SWT.BORDER);
			txtMappingAttrName.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		}
		else {
			new Label(panDialogArea, SWT.NONE);
			new Label(panDialogArea, SWT.NONE);
		}

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

		if (mode == Mode.MAPPED) {
			final var lblDefaultValue = new Label(panDialogArea, SWT.NONE);
			lblDefaultValue.setText("Default value:");

			txtDefault = new Text(panDialogArea, SWT.BORDER);
			txtDefault.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1));
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

		newShell.setText(DLG_TITLE);
	}

}
