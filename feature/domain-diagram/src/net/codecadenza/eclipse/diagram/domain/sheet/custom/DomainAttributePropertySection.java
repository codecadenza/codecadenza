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

import net.codecadenza.eclipse.diagram.domain.edit.parts.DomainAttributeEditPart;
import net.codecadenza.eclipse.diagram.domain.part.CodeCadenzaDiagramEditorPlugin;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.service.domain.DomainObjectService;
import org.eclipse.emf.transaction.RecordingCommand;
import org.eclipse.gmf.runtime.notation.Node;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.CLabel;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.views.properties.tabbed.AbstractPropertySection;
import org.eclipse.ui.views.properties.tabbed.ITabbedPropertyConstants;
import org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage;

/**
 * <p>
 * Property section for domain attributes
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainAttributePropertySection extends AbstractPropertySection {
	private static final String DLG_TITLE = "Edit domain attribute";
	private static final int LABEL_WIDTH = 220;

	private Text txtName;
	private Text txtLabel;
	private Text txtLabelPlural;
	private Button chkInsertable;
	private Button chkUpdatable;
	private Button chkFetchTypeEager;
	private Button chkVersion;
	private Button chkNullable;
	private Button chkDisplayAttribute;
	private Text txtMinLength;
	private Text txtMaxLength;
	private Text txtMinValue;
	private Text txtMaxValue;
	private Button chkRemoveWhitespaceCharacters;
	private Button chkConvertToUpperCase;
	private Button chkConvertToLowerCase;
	private DomainAttribute attribute;
	private DomainAttributeEditPart part;
	private ModifyListener textChangeListener;

	/**
	 * Listener for responding to selection changes
	 */
	private class SelectionChangeListener extends SelectionAdapter {
		private final boolean rebuild;

		/**
		 * @param rebuild
		 */
		public SelectionChangeListener(boolean rebuild) {
			this.rebuild = rebuild;
		}

		/*
		 * (non-Javadoc)
		 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
		 */
		@Override
		public void widgetSelected(SelectionEvent e) {
			updateMetaModel(rebuild);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#createControls(org.eclipse.swt.widgets.Composite,
	 * org.eclipse.ui.views.properties.tabbed.TabbedPropertySheetPage)
	 */
	@Override
	public void createControls(Composite parent, TabbedPropertySheetPage tabbedPropertySheetPage) {
		super.createControls(parent, tabbedPropertySheetPage);

		final Composite panPropertySection = getWidgetFactory().createFlatFormComposite(parent);

		var data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(0, ITabbedPropertyConstants.VSPACE);

		txtName = getWidgetFactory().createText(panPropertySection, "", SWT.READ_ONLY);
		txtName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtName, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtName, 0, SWT.CENTER);

		final CLabel lblName = getWidgetFactory().createCLabel(panPropertySection, "Name:");
		lblName.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtName, ITabbedPropertyConstants.VSPACE);

		txtLabel = getWidgetFactory().createText(panPropertySection, "");
		txtLabel.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtLabel, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtLabel, 0, SWT.CENTER);

		final CLabel lblLabel = getWidgetFactory().createCLabel(panPropertySection, "Label:");
		lblLabel.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtLabel, ITabbedPropertyConstants.VSPACE);

		txtLabelPlural = getWidgetFactory().createText(panPropertySection, "");
		txtLabelPlural.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtLabelPlural, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtLabelPlural, 0, SWT.CENTER);

		final CLabel lblLabelPlural = getWidgetFactory().createCLabel(panPropertySection, "Label plural:");
		lblLabelPlural.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtLabelPlural, ITabbedPropertyConstants.VSPACE);

		chkInsertable = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkInsertable.setLayoutData(data);
		chkInsertable.addSelectionListener(new SelectionChangeListener(true));

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkInsertable, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkInsertable, 0, SWT.CENTER);

		final CLabel lblInsertable = getWidgetFactory().createCLabel(panPropertySection, "Insertable:");
		lblInsertable.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkInsertable, ITabbedPropertyConstants.VSPACE);

		chkUpdatable = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkUpdatable.setLayoutData(data);
		chkUpdatable.addSelectionListener(new SelectionChangeListener(true));

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkUpdatable, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkUpdatable, 0, SWT.CENTER);

		final CLabel lblUpdatable = getWidgetFactory().createCLabel(panPropertySection, "Updatable:");
		lblUpdatable.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkUpdatable, ITabbedPropertyConstants.VSPACE);

		chkFetchTypeEager = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkFetchTypeEager.setLayoutData(data);
		chkFetchTypeEager.addSelectionListener(new SelectionChangeListener(true));

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkFetchTypeEager, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkFetchTypeEager, 0, SWT.CENTER);

		final CLabel lblFetchTypeEager = getWidgetFactory().createCLabel(panPropertySection, "Eager fetch:");
		lblFetchTypeEager.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkFetchTypeEager, ITabbedPropertyConstants.VSPACE);

		chkVersion = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkVersion.setLayoutData(data);
		chkVersion.addSelectionListener(new SelectionChangeListener(true));

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkVersion, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkVersion, 0, SWT.CENTER);

		final CLabel lblVersion = getWidgetFactory().createCLabel(panPropertySection, "Version:");
		lblVersion.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkVersion, ITabbedPropertyConstants.VSPACE);

		chkNullable = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkNullable.setLayoutData(data);
		chkNullable.addSelectionListener(new SelectionChangeListener(true));

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkNullable, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkNullable, 0, SWT.CENTER);

		final CLabel lblNullable = getWidgetFactory().createCLabel(panPropertySection, "Nullable:");
		lblNullable.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkNullable, ITabbedPropertyConstants.VSPACE);

		chkDisplayAttribute = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkDisplayAttribute.setLayoutData(data);
		chkDisplayAttribute.addSelectionListener(new SelectionChangeListener(true));

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkDisplayAttribute, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkDisplayAttribute, 0, SWT.CENTER);

		final CLabel lblDisplayAttribute = getWidgetFactory().createCLabel(panPropertySection, "Display attribute:");
		lblDisplayAttribute.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkDisplayAttribute, ITabbedPropertyConstants.VSPACE);

		txtMinLength = getWidgetFactory().createText(panPropertySection, "");
		txtMinLength.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtMinLength, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtMinLength, 0, SWT.CENTER);

		final CLabel lblMinLength = getWidgetFactory().createCLabel(panPropertySection, "Min. length:");
		lblMinLength.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtMinLength, ITabbedPropertyConstants.VSPACE);

		txtMaxLength = getWidgetFactory().createText(panPropertySection, "");
		txtMaxLength.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtMaxLength, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtMaxLength, 0, SWT.CENTER);

		final CLabel lblMaxLength = getWidgetFactory().createCLabel(panPropertySection, "Max. lenth:");
		lblMaxLength.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtMaxLength, ITabbedPropertyConstants.VSPACE);

		txtMinValue = getWidgetFactory().createText(panPropertySection, "");
		txtMinValue.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtMinValue, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtMinValue, 0, SWT.CENTER);

		final CLabel lblMinValue = getWidgetFactory().createCLabel(panPropertySection, "Min. value:");
		lblMinValue.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtMinValue, ITabbedPropertyConstants.VSPACE);

		txtMaxValue = getWidgetFactory().createText(panPropertySection, "");
		txtMaxValue.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(txtMaxValue, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(txtMaxValue, 0, SWT.CENTER);

		final CLabel lblMaxValue = getWidgetFactory().createCLabel(panPropertySection, "Max. value:");
		lblMaxValue.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(txtMaxValue, ITabbedPropertyConstants.VSPACE);

		chkRemoveWhitespaceCharacters = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkRemoveWhitespaceCharacters.setLayoutData(data);
		chkRemoveWhitespaceCharacters.addSelectionListener(new SelectionChangeListener(false));

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkRemoveWhitespaceCharacters, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkRemoveWhitespaceCharacters, 0, SWT.CENTER);

		final CLabel lblRemoveWhitespaceChars = getWidgetFactory().createCLabel(panPropertySection, "Remove whitespace characters:");
		lblRemoveWhitespaceChars.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkRemoveWhitespaceCharacters, ITabbedPropertyConstants.VSPACE);

		chkConvertToUpperCase = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkConvertToUpperCase.setLayoutData(data);

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

				updateMetaModel(false);
			}
		});

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkConvertToUpperCase, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkConvertToUpperCase, 0, SWT.CENTER);

		final CLabel lblConvertToUpperCase = getWidgetFactory().createCLabel(panPropertySection, "Convert to upper-case:");
		lblConvertToUpperCase.setLayoutData(data);

		data = new FormData();
		data.left = new FormAttachment(0, LABEL_WIDTH);
		data.right = new FormAttachment(0, 400);
		data.top = new FormAttachment(chkConvertToUpperCase, ITabbedPropertyConstants.VSPACE);

		chkConvertToLowerCase = getWidgetFactory().createButton(panPropertySection, "", SWT.CHECK);
		chkConvertToLowerCase.setLayoutData(data);

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

				updateMetaModel(false);
			}
		});

		data = new FormData();
		data.left = new FormAttachment(0, 0);
		data.right = new FormAttachment(chkConvertToLowerCase, -ITabbedPropertyConstants.HSPACE);
		data.top = new FormAttachment(chkConvertToLowerCase, 0, SWT.CENTER);

		final CLabel lblConvertToLowerCase = getWidgetFactory().createCLabel(panPropertySection, "Convert to lower-case:");
		lblConvertToLowerCase.setLayoutData(data);

		textChangeListener = _ -> updateMetaModel(true);
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
		this.part = (DomainAttributeEditPart) input;
		this.attribute = (DomainAttribute) ((Node) ((DomainAttributeEditPart) input).getModel()).getElement();
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ui.views.properties.tabbed.AbstractPropertySection#refresh()
	 */
	@Override
	public void refresh() {
		final JavaType type = attribute.getJavaType();

		resetFields();

		txtName.setText(attribute.getName());
		txtLabel.setText(attribute.getLabel());
		txtLabelPlural.setText(attribute.getLabelPlural());

		txtLabel.addModifyListener(textChangeListener);
		txtLabelPlural.addModifyListener(textChangeListener);

		if (!attribute.isPersistent())
			return;

		chkFetchTypeEager.setEnabled(true);
		chkFetchTypeEager.setSelection(attribute.isFetchTypeEager());
		chkInsertable.setEnabled(true);
		chkInsertable.setSelection(attribute.isInsertable());
		chkUpdatable.setEnabled(true);
		chkUpdatable.setSelection(attribute.isUpdatable());

		if (attribute.getCollectionType() != CollectionTypeEnumeration.NONE)
			return;

		if (!attribute.getJavaType().isPrimitive() && !attribute.isPk()) {
			chkNullable.setEnabled(true);
			chkNullable.setSelection(attribute.getDomainAttributeValidator().isNullable());
		}

		if (!attribute.isPk() && type.isIntegerOrLong()) {
			chkVersion.setSelection(attribute.isTrackVersion());
			chkVersion.setEnabled(true);
		}

		if (type.isString() || type.isByteArray()) {
			if (type.isString()) {
				chkDisplayAttribute.setEnabled(true);
				chkDisplayAttribute.setSelection(attribute.isDisplayAttribute());
				chkRemoveWhitespaceCharacters.setEnabled(true);
				chkRemoveWhitespaceCharacters.setSelection(attribute.isRemoveWhitespaceCharacters());
				chkConvertToUpperCase.setEnabled(true);
				chkConvertToUpperCase.setSelection(attribute.isConvertToUpperCase());
				chkConvertToLowerCase.setEnabled(true);
				chkConvertToLowerCase.setSelection(attribute.isConvertToLowerCase());
			}

			txtMinLength.setEnabled(true);

			if (attribute.getDomainAttributeValidator().getMinLength() != null)
				txtMinLength.setText(String.valueOf(attribute.getDomainAttributeValidator().getMinLength()));

			txtMinLength.addModifyListener(textChangeListener);

			txtMaxLength.setEnabled(true);

			if (attribute.getDomainAttributeValidator().getMaxLength() != null)
				txtMaxLength.setText(String.valueOf(attribute.getDomainAttributeValidator().getMaxLength()));

			txtMaxLength.addModifyListener(textChangeListener);
		}
		else if (type.isNumber()) {
			txtMinValue.setEnabled(true);
			txtMinValue.setText(attribute.getDomainAttributeValidator().getMinValue());
			txtMinValue.addModifyListener(textChangeListener);
			txtMaxValue.setEnabled(true);
			txtMaxValue.setText(attribute.getDomainAttributeValidator().getMaxValue());
			txtMaxValue.addModifyListener(textChangeListener);
		}
	}

	/**
	 * Validate the input, change the meta-model and rebuild the respective domain object source files
	 * @param performRebuild
	 */
	public void updateMetaModel(boolean performRebuild) {
		final DomainObject domainObject = attribute.getDomainObject();
		Integer minLength = null;
		Integer maxLength = null;

		if (txtLabel.getText().isEmpty()) {
			showValidationMessage("The label must not be empty!");
			txtLabel.setFocus();
			return;
		}

		if (txtLabelPlural.getText().isEmpty()) {
			showValidationMessage("The plural form of the label must not be empty!");
			txtLabelPlural.setFocus();
			return;
		}

		try {
			if (txtMinLength.isEnabled() && !txtMinLength.getText().isEmpty()) {
				minLength = Integer.parseInt(txtMinLength.getText());

				if (minLength < 0)
					throw new IllegalArgumentException();
			}
		}
		catch (final Exception _) {
			showValidationMessage("The min. length requires an integer value!");
			txtMinLength.setFocus();
			return;
		}

		try {
			final boolean requiresLength = !attribute.getColumn().getColumnType().isOmitSizeInformation();

			if (txtMaxLength.isEnabled() && (requiresLength || !txtMaxLength.getText().isEmpty())) {
				maxLength = Integer.parseInt(txtMaxLength.getText());

				if (maxLength < 1)
					throw new IllegalArgumentException();
			}
		}
		catch (final Exception _) {
			showValidationMessage("The max. length requires an integer value!");
			txtMaxLength.setFocus();
			return;
		}

		if (maxLength != null && minLength != null && minLength > maxLength) {
			showValidationMessage("The max. length must be greater than the min. length!");
			txtMaxLength.setFocus();
			return;
		}

		// Test if another version attribute already exists!
		if (chkVersion.getSelection())
			for (final DomainAttribute attr : domainObject.getAllAttributes())
				if (!attr.equals(attribute) && attr.isTrackVersion()) {
					showValidationMessage("A version attribute with the name '" + attr.getName() + "' already exists!");
					chkVersion.setFocus();
					return;
				}

		// Test if another display attribute already exists!
		if (chkDisplayAttribute.getSelection())
			for (final DomainAttribute attr : domainObject.getAllAttributes())
				if (!attr.equals(attribute) && attr.isDisplayAttribute()) {
					showValidationMessage("A display attribute with the name '" + attr.getName() + "' already exists!");
					chkDisplayAttribute.setFocus();
					return;
				}

		// Save the changes
		part.getEditingDomain().getCommandStack().execute(new RecordingCommand(part.getEditingDomain()) {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.emf.transaction.RecordingCommand#doExecute()
			 */
			@Override
			protected void doExecute() {
				attribute.setLabel(txtLabel.getText());
				attribute.setLabelPlural(txtLabelPlural.getText());

				if (!attribute.isPersistent())
					return;

				attribute.setInsertable(chkInsertable.getSelection());
				attribute.setUpdatable(chkUpdatable.getSelection());
				attribute.setFetchTypeEager(chkFetchTypeEager.getSelection());
				attribute.setTrackVersion(chkVersion.getSelection());
				attribute.setDisplayAttribute(chkDisplayAttribute.getSelection());
				attribute.setRemoveWhitespaceCharacters(chkRemoveWhitespaceCharacters.getSelection());
				attribute.setConvertToUpperCase(chkConvertToUpperCase.getSelection());
				attribute.setConvertToLowerCase(chkConvertToLowerCase.getSelection());

				final DomainAttributeValidator validator = attribute.getDomainAttributeValidator();

				if (txtMinLength.isEnabled()) {
					if (!txtMinLength.getText().isEmpty())
						validator.setMinLength(Integer.parseInt(txtMinLength.getText()));
					else
						validator.setMinLength(null);
				}

				if (txtMaxLength.isEnabled()) {
					if (!txtMaxLength.getText().isEmpty()) {
						validator.setMaxLength(Integer.parseInt(txtMaxLength.getText()));
						attribute.getColumn().setLength(validator.getMaxLength());
					}
					else {
						validator.setMaxLength(null);
						attribute.getColumn().setLength(0);
					}
				}

				if (chkNullable.isEnabled()) {
					validator.setNullable(chkNullable.getSelection());
					attribute.getColumn().setNullable(chkNullable.getSelection());
				}

				if (txtMinValue.isEnabled())
					validator.setMinValue(txtMinValue.getText());

				if (txtMaxValue.isEnabled())
					validator.setMaxValue(txtMaxValue.getText());
			}
		});

		// Rebuild the respective domain object source files
		if (performRebuild)
			try {
				final var domainObjectService = new DomainObjectService(attribute.getDomainObject().getNamespace().getProject());
				domainObjectService.rebuildDomainObjectSourceFiles(attribute.getDomainObject(), false);
			}
			catch (final Exception e) {
				CodeCadenzaDiagramEditorPlugin.getInstance().handleInternalError(e);
			}
	}

	/**
	 * Reset all fields and remove the listeners
	 */
	private void resetFields() {
		// Remove the listeners
		txtLabel.removeModifyListener(textChangeListener);
		txtLabelPlural.removeModifyListener(textChangeListener);
		txtMinLength.removeModifyListener(textChangeListener);
		txtMaxLength.removeModifyListener(textChangeListener);
		txtMinValue.removeModifyListener(textChangeListener);
		txtMaxValue.removeModifyListener(textChangeListener);

		// Reset and disable fields that depend on the attribute configuration
		chkFetchTypeEager.setEnabled(false);
		chkFetchTypeEager.setSelection(false);
		chkInsertable.setEnabled(false);
		chkInsertable.setSelection(false);
		chkUpdatable.setEnabled(false);
		chkUpdatable.setSelection(false);
		chkVersion.setEnabled(false);
		chkVersion.setSelection(false);
		chkDisplayAttribute.setEnabled(false);
		chkDisplayAttribute.setSelection(false);
		txtMinLength.setText("");
		txtMinLength.setEnabled(false);
		txtMaxLength.setText("");
		txtMaxLength.setEnabled(false);
		txtMinValue.setText("");
		txtMinValue.setEnabled(false);
		txtMaxValue.setText("");
		txtMaxValue.setEnabled(false);
		chkNullable.setEnabled(false);
		chkNullable.setSelection(false);
		chkRemoveWhitespaceCharacters.setEnabled(false);
		chkRemoveWhitespaceCharacters.setSelection(false);
		chkConvertToUpperCase.setEnabled(false);
		chkConvertToUpperCase.setSelection(false);
		chkConvertToLowerCase.setEnabled(false);
		chkConvertToLowerCase.setSelection(false);
	}

	/**
	 * @param validationMessage
	 */
	private void showValidationMessage(String validationMessage) {
		final Shell shell = Display.getCurrent().getActiveShell();

		MessageDialog.openInformation(shell, DLG_TITLE, validationMessage);
		refresh();
	}

}
