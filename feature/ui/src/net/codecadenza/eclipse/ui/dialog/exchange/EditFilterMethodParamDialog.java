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

import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainFactory;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;

/**
 * <p>
 * Dialog for creating and maintaining filter parameters for data exchange methods
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditFilterMethodParamDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE_EDIT = "Edit filter parameter";
	private static final String DLG_TITLE_NEW = "Create new filter parameter";

	private static final String OPERATOR_LIKE = "like";
	private static final String OPERATOR_NOTLIKE = "not like";
	private static final String OPERATOR_EQUAL = "=";
	private static final String OPERATOR_NOT_EQUAL = "!=";
	private static final String OPERATOR_GREATER = ">";
	private static final String OPERATOR_SMALLER = "<";
	private static final String OPERATOR_GREATER_OR_EQUAL = ">=";
	private static final String OPERATOR_SMALLER_OR_EQUAL = "<=";

	private Text txtName;
	private Combo cboOperator;
	private DataComboViewer<DomainAttribute> cboAttribute;
	private DataComboViewer<AbstractDomainAssociation> cboAssociation;
	private final boolean doEdit;
	private final DomainObject domainObject;
	private FilterMethodParameter filterParameter;
	private final String title;

	/**
	 * Constructor
	 * @param parentShell
	 * @param domainObject
	 * @param filterParameter
	 */
	public EditFilterMethodParamDialog(Shell parentShell, DomainObject domainObject, FilterMethodParameter filterParameter) {
		super(parentShell);

		this.doEdit = true;
		this.filterParameter = filterParameter;
		this.domainObject = domainObject;
		this.title = DLG_TITLE_EDIT;
	}

	/**
	 * Constructor
	 * @param parentShell
	 * @param domainObject
	 */
	public EditFilterMethodParamDialog(Shell parentShell, DomainObject domainObject) {
		super(parentShell);

		this.doEdit = false;
		this.domainObject = domainObject;
		this.title = DLG_TITLE_NEW;
	}

	/**
	 * @return the filter parameter
	 */
	public FilterMethodParameter getFilterParameter() {
		return filterParameter;
	}

	/**
	 * @param obj
	 * @return a list containing all supported attributes of the given domain object
	 */
	private BasicEList<DomainAttribute> getValidAttributes(DomainObject obj) {
		final var attrList = new BasicEList<DomainAttribute>();

		// Non-persistent, element collection and LOB attributes are not supported!
		for (final DomainAttribute attr : obj.getAllAttributes())
			if (!attr.isLob() && attr.isPersistent() && attr.getCollectionType() == CollectionTypeEnumeration.NONE)
				attrList.add(attr);

		return attrList;
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

		final var gdName = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdName.widthHint = 150;

		txtName = new Text(panDialogArea, SWT.BORDER);
		txtName.setLayoutData(gdName);

		final var lblOperator = new Label(panDialogArea, SWT.NONE);
		lblOperator.setText("Operator:");

		final var gdOperator = new GridData(SWT.FILL, SWT.CENTER, true, false);
		gdOperator.widthHint = 150;

		cboOperator = new Combo(panDialogArea, SWT.NONE | SWT.READ_ONLY);
		cboOperator.setLayoutData(gdOperator);
		cboOperator.add(OPERATOR_LIKE);
		cboOperator.add(OPERATOR_NOTLIKE);
		cboOperator.add(OPERATOR_EQUAL);
		cboOperator.add(OPERATOR_NOT_EQUAL);
		cboOperator.add(OPERATOR_GREATER);
		cboOperator.add(OPERATOR_GREATER_OR_EQUAL);
		cboOperator.add(OPERATOR_SMALLER);
		cboOperator.add(OPERATOR_SMALLER_OR_EQUAL);

		final var lblAssociation = new Label(panDialogArea, SWT.NONE);
		lblAssociation.setText("Domain association:");

		cboAssociation = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(AbstractDomainAssociation element) {
				return element.getName();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#onSelectionChanged(java.lang.Object)
			 */
			@Override
			public void onSelectionChanged(AbstractDomainAssociation element) {
				if (!element.getName().isEmpty())
					cboAttribute.setData(getValidAttributes(element.getTarget()));
				else
					cboAttribute.setData(getValidAttributes(domainObject));
			}
		};

		cboAssociation.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));

		final var lblDomainAttribute = new Label(panDialogArea, SWT.NONE);
		lblDomainAttribute.setText("Domain attribute:");

		cboAttribute = new DataComboViewer<>(panDialogArea, SWT.BORDER | SWT.READ_ONLY) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.runtime.richclient.eclipse.widget.DataComboViewer#getItemText(java.lang.Object)
			 */
			@Override
			public String getItemText(DomainAttribute element) {
				return element.getName();
			}
		};

		cboAttribute.setLayoutData(new GridData(SWT.FILL, SWT.CENTER, true, false));
		cboAttribute.setData(getValidAttributes(domainObject));

		final var assocList = new BasicEList<AbstractDomainAssociation>();
		final ManyToOneAssociation defaultAssoc = DomainFactory.eINSTANCE.createManyToOneAssociation();
		defaultAssoc.setName("");

		assocList.add(defaultAssoc);

		// Many-to-many and one-to-many associations are not supported!
		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations())
			if (assoc instanceof ManyToOneAssociation || assoc instanceof OneToOneAssociation)
				assocList.add(assoc);

		cboAssociation.setData(assocList);

		if (doEdit) {
			txtName.setText(filterParameter.getName());
			cboOperator.select(cboOperator.indexOf(filterParameter.getOperator()));

			if (filterParameter.getAssociation() != null)
				cboAssociation.setSelectedItem(filterParameter.getAssociation());
			else
				cboAssociation.setSelectedItem(defaultAssoc);

			if (filterParameter.getAssociation() != null)
				cboAttribute.setData(getValidAttributes(filterParameter.getAssociation().getTarget()));

			cboAttribute.setSelectedItem(filterParameter.getDomainAttribute());
		}
		else {
			cboOperator.select(0);
			cboAssociation.setSelectedItem(defaultAssoc);
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
			// The name of the parameter must follow Java naming conventions!
			final IStatus status = EclipseIDEService.validateTypeVariableName(txtName.getText());

			if (status.getSeverity() > IStatus.INFO) {
				MessageDialog.openInformation(getShell(), title, status.getMessage());
				txtName.setFocus();
				return;
			}

			if (cboAttribute.getSelectedItem() == null) {
				MessageDialog.openInformation(getShell(), title, "An attribute must be selected!");
				cboAttribute.setFocus();
				return;
			}

			if (!doEdit)
				filterParameter = ExchangeFactory.eINSTANCE.createFilterMethodParameter();

			filterParameter.setName(txtName.getText());
			filterParameter.setModifier(JavaTypeModifierEnumeration.NONE);
			filterParameter.setOperator(cboOperator.getItem(cboOperator.getSelectionIndex()));
			filterParameter.setDomainAttribute(cboAttribute.getSelectedItem());
			filterParameter.setType(filterParameter.getDomainAttribute().getJavaType());

			if (!cboAssociation.getSelectedItem().getName().isEmpty())
				filterParameter.setAssociation(cboAssociation.getSelectedItem());
			else
				filterParameter.setAssociation(null);
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

		newShell.setText(title);
	}

}
