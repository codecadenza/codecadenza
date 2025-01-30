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
package net.codecadenza.eclipse.ui.dialog.client;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.service.form.init.UpdateFormInitService;
import net.codecadenza.eclipse.shared.dialog.CodeCadenzaDialog;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.panel.DomainObjectTreePanel;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.TreeItem;

/**
 * <p>
 * Dialog for adding either a new form field or an initial one-to-many association to a form panel
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class NewFormFieldDialog extends CodeCadenzaDialog {
	private static final String DLG_TITLE = "Create new form field";

	private final UpdateFormInitService initService;
	private DomainObjectTreePanel domainObjectTreePanel;
	private final FormPanel formPanel;
	private final DTOBean dto;
	private final Form form;
	private final FormTypeEnumeration formType;
	private boolean fieldMode;

	/**
	 * Constructor
	 * @param parentShell
	 * @param formPanel
	 * @param initService
	 * @param editMode
	 */
	public NewFormFieldDialog(Shell parentShell, FormPanel formPanel, UpdateFormInitService initService, boolean editMode) {
		super(parentShell);

		this.formPanel = formPanel;
		this.form = formPanel.getForm();
		this.formType = form.getFormType();
		this.dto = form.getDTO();
		this.initService = initService;

		// When creating new forms of type 'ADD' or 'CREATE' only initial one-to-many associations can be selected!
		if (editMode || formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
			this.fieldMode = true;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected Control createDialogArea(Composite parent) {
		final var panDialogArea = (Composite) super.createDialogArea(parent, 1, false);

		domainObjectTreePanel = new DomainObjectTreePanel(panDialogArea) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.util.DomainObjectTreePanel#addDomainAttribute(net.codecadenza.eclipse.model.domain.
			 * DomainAttribute)
			 */
			@Override
			protected boolean addDomainAttribute(DomainAttribute attr) {
				if (!fieldMode)
					return false;

				if (formType == FormTypeEnumeration.CREATE
						|| formType == FormTypeEnumeration.ADD && (!attr.isPersistent() || attr.isSetDateOnUpdate()))
					return false;

				return !attr.isLob();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.util.DomainObjectTreePanel#addDomainAssociation(net.codecadenza.eclipse.model.
			 * domain.AbstractDomainAssociation)
			 */
			@Override
			protected boolean addDomainAssociation(AbstractDomainAssociation assoc) {
				if (fieldMode) {
					if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation)
						return false;
				}
				else if (assoc instanceof OneToManyAssociation) {
					// Check if a panel for that association already exists!
					for (final FormPanel panel : form.getFormPanels())
						if (panel.getDTO() != null && panel.getDTO().getDomainObject().equals(assoc.getTarget()))
							return false;

					// Domain objects that are tagged with 'DOCUMENT' cannot be supported at that point!
					return assoc.getTarget().getTag() != DomainTagEnumeration.DOCUMENT;
				}
				else
					return false;

				return true;
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.util.DomainObjectTreePanel#onMouseDoubleClick(org.eclipse.swt.widgets.TreeItem)
			 */
			@Override
			protected void onMouseDoubleClick(TreeItem selectedItem) {
				if (selectedItem == null || selectedItem.getData() == null || selectedItem.getData() instanceof DomainObject)
					return;

				try {
					if (fieldMode) {
						final boolean fieldAdded = addNewField(selectedItem);

						if (!fieldAdded) {
							MessageDialog.openInformation(getShell(), DLG_TITLE, "A field with the same mapping already exists!");
							return;
						}
					}

					if (!fieldMode && selectedItem.getData() instanceof final OneToManyAssociation otm)
						initService.createInitialOneToManyPanel(otm);
				}
				catch (final Exception ex) {
					CodeCadenzaUserInterfacePlugin.getInstance().handleInternalError(ex);
					return;
				}

				setReturnCode(Dialog.OK);
				close();
			}
		};

		final var gdDomainObjectTreePanel = new GridData(SWT.FILL, SWT.FILL, true, true);
		gdDomainObjectTreePanel.widthHint = 400;
		gdDomainObjectTreePanel.heightHint = 500;

		domainObjectTreePanel.setLayoutData(gdDomainObjectTreePanel);
		domainObjectTreePanel.setStandardConversion(true);
		domainObjectTreePanel.init(dto.getDomainObject());

		return panDialogArea;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)
	 */
	@Override
	protected void createButtonsForButtonBar(Composite parent) {
		createButton(parent, IDialogConstants.CANCEL_ID, IDialogConstants.CANCEL_LABEL, false);
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)
	 */
	@Override
	protected void configureShell(Shell newShell) {
		super.configureShell(newShell);

		if (fieldMode)
			newShell.setText("Select field to be added...");
		else
			newShell.setText("Select initial one-to-many association to be added...");
	}

	/**
	 * Add a new field to the existing form panel
	 * @param selItem
	 * @return true if the field has been added
	 */
	private boolean addNewField(TreeItem selItem) {
		final List<AbstractDomainAssociation> fullAssocList = domainObjectTreePanel.getAssociationListOfSelectedTreeItem();
		final var assocList = new ArrayList<AbstractDomainAssociation>();
		final AbstractDomainAssociation assoc = !fullAssocList.isEmpty() ? fullAssocList.get(0) : null;

		// Do not add the full association list! Just add all elements after the first one!
		if (fullAssocList.size() > 1)
			for (int i = 1; i < fullAssocList.size(); i++)
				assocList.add(fullAssocList.get(i));

		if (selItem.getData() instanceof final DomainAttribute selAttr) {
			final boolean forceReadonly = !selAttr.isPersistent() || selAttr.isSetDateOnPersist() || selAttr.isSetDateOnUpdate();

			if (assoc != null) {
				final String labelPrefix = assoc.getName().substring(0, 1).toUpperCase() + assoc.getName().substring(1);

				// A new field should not be added if another form field is mapped to a DTO attribute with the same mapping!
				if (!dto.isShared() && duplicateMappingExists(selAttr, assoc, assocList))
					return false;

				initService.addFormField(dto, formPanel, selAttr, labelPrefix, forceReadonly, fullAssocList);
			}
			else {
				// A new field should not be added if another form field is mapped to a DTO attribute with the same mapping!
				if (!dto.isShared() && duplicateMappingExists(selAttr, null, null))
					return false;

				initService.addFormField(dto, formPanel, selAttr, "", forceReadonly, null);
			}
		}
		else if (selItem.getData() instanceof ManyToOneAssociation) {
			if (!dto.isShared() && duplicateMappingExists(assoc, assocList))
				return false;

			initService.addFormList(dto, formPanel, fullAssocList);
		}

		return true;
	}

	/**
	 * Check if a duplicate DTO attribute exists that is mapped to a form field
	 * @param domainAttribute
	 * @param assoc
	 * @param assocList
	 * @return true if the form already contains a field with the same mapping
	 */
	private boolean duplicateMappingExists(DomainAttribute domainAttribute, AbstractDomainAssociation assoc,
			List<AbstractDomainAssociation> assocList) {
		for (final FormField field : form.getAllFormFields()) {
			final DTOBeanAttribute dtoAttr = field.getDTOAttribute();

			if (dtoAttr.getDTOBean().searchAttribute(domainAttribute, assoc, assocList) != null)
				return true;
		}

		return false;
	}

	/**
	 * Check if a duplicate DTO attribute exists that is mapped to a form list field
	 * @param assoc
	 * @param assocList
	 * @return true if the form already contains a list field with the same mapping
	 */
	private boolean duplicateMappingExists(AbstractDomainAssociation assoc, List<AbstractDomainAssociation> assocList) {
		for (final FormField field : form.getAllFormFields()) {
			final DTOBeanAttribute dtoAttr = field.getDTOAttribute();

			if (dtoAttr.getReferencedDTOBean() == null)
				continue;

			if (dtoAttr.getDTOBean().searchAttribute(dtoAttr.getReferencedDTOBean(), assoc, assocList) != null)
				return true;
		}

		return false;
	}

}
