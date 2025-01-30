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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form.field;

import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.LOV_PREFIX;

import net.codecadenza.eclipse.generator.client.common.field.AbstractRichClientFieldGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.security.EclipseSecurityHelper;
import net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;

/**
 * <p>
 * Abstract base class for all Eclipse RCP/RAP form field generators
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public abstract class AbstractEclipseFieldGenerator extends AbstractRichClientFieldGenerator {
	private static final String LABEL_PREFIX = "lblFor";

	protected String upperCaseFieldName;

	/**
	 * Constructor
	 * @param field
	 * @param formGenerator
	 */
	protected AbstractEclipseFieldGenerator(FormField field, AbstractJavaSourceGenerator formGenerator) {
		super(field, formGenerator);

		this.upperCaseFieldName = field.getName().substring(0, 1).toUpperCase() + field.getName().substring(1);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractClientFieldGenerator#getFieldDefinitionFragment(boolean)
	 */
	@Override
	public String getFieldDefinitionFragment(boolean hasOneColumn) {
		if (!field.isVisible())
			return "";

		final var b = new StringBuilder();
		final var gridDataName = "gd" + upperCaseFieldName;

		if (field.getColIndex() == 2)
			b.append(fillEmptyColumns());

		if (field.isAddFormLinkToLabel()) {
			b.append("final var " + getFieldLabelName() + " = new Link(" + field.getPanel().getName() + ", SWT.NONE);\n");
			b.append(getFieldLabelName() + ".setText(\"<a>\" + " + i18n.getI18N(field, true) + " + \"</a>\");\n");
			b.append(createCommonFormLinkFragment(getFieldLabelName()));
		}
		else {
			b.append("final var " + getFieldLabelName() + " = new Label(" + field.getPanel().getName() + ", SWT.NONE);\n");
			b.append(getFieldLabelName() + ".setText(" + i18n.getI18N(field, true) + ");\n");

			if (field.isMandatory() && !field.isReadonly())
				b.append(getFieldLabelName() + ".setForeground(Display.getCurrent().getSystemColor(SWT.COLOR_BLUE));\n");

			b.append("\n");
		}

		if (addLayout())
			b.append(addFieldLayout(gridDataName, hasOneColumn));

		b.append(getFieldDefinitionFragment());

		if (addLayout())
			b.append(field.getName() + ".setLayoutData(" + gridDataName + ");\n");

		b.append(addToolTipFragment());

		if (!hasOneColumn && field.getColIndex() == 1)
			b.append(fillEmptyColumns());

		b.append("\n");

		return b.toString();
	}

	/**
	 * @return the name of the field's label
	 */
	public String getFieldLabelName() {
		return LABEL_PREFIX + upperCaseFieldName;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.client.common.field.AbstractRichClientFieldGenerator#
	 * getFieldValidationMessageFragment(java.lang.String, boolean)
	 */
	@Override
	protected String getFieldValidationMessageFragment(String message, boolean hasTitleArea) {
		final var b = new StringBuilder();
		b.append("{\n");
		b.append("final String " + FIELD_LABEL_VALIDATION + " = " + i18n.getI18N(field) + ";\n");

		if (hasTitleArea) {
			b.append("setErrorMessage(" + message + ");\n");
			b.append(field.getName() + ".setFocus();\n");
			b.append("return false;\n");
		}
		else
			b.append("fieldErrors.add(new FieldValidationBean(" + FIELD_LABEL_VALIDATION + ", " + message + "));\n");

		b.append("}\n");

		return b.toString();
	}

	/**
	 * @return true if the layout data should be added to this field
	 */
	protected boolean addLayout() {
		return true;
	}

	/**
	 * @return the generated content
	 */
	protected abstract String getFieldDefinitionFragment();

	/**
	 * @param fieldName
	 * @return the generated content
	 */
	protected String createCommonFormLinkFragment(String fieldName) {
		final var b = new StringBuilder();
		final String getter = field.getDTOAttribute().getModelGetterName();
		final var securityHelper = new EclipseSecurityHelper(project);
		final DTOBeanAttribute pkAttr = field.getDTOAttribute().getReferencedDTOBean().getPKAttribute();
		Form readonlyForm = null;

		for (final Form f : project.getAllFormsOfProject()) {
			if (!f.getDomainObject().equals(field.getDTOAttribute().getReferencedDTOBean().getDomainObject()))
				continue;

			if (f.getFormType() == FormTypeEnumeration.READONLY) {
				readonlyForm = f;
				break;
			}
		}

		b.append("\n");
		b.append(fieldName + ".addMouseListener(new MouseAdapter()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("public void mouseDown(MouseEvent e)\n");
		b.append("{\n");

		if (readonlyForm != null) {
			final var openForm = new StringBuilder();
			final var msg = "Cannot open respective detail form because of missing selection value for this field!";

			if (field.getFieldType() == FormFieldTypeEnumeration.FORM_LINK || field.isReadonly()) {
				// We create a field generator temporarily in order to create a check-fragment
				final var fieldGenerator = new EclipseComboboxFieldGenerator(field, formGenerator);

				fieldGenerator.setModelObjectName(field.getPanel().getForm().getDTO().getDomainObject().getLowerCaseName());
				final String checkFragment = fieldGenerator.getCheckFragment();

				openForm.append(checkFragment);

				if (!checkFragment.isEmpty())
					openForm.append("{\n");

				openForm.append("final var dlg = new " + readonlyForm.getName());
				openForm.append("(parentShell, " + modelObjectName + "." + getter + "." + pkAttr.getModelGetterName() + ");\n");
				openForm.append("dlg.open();\n");

				if (!checkFragment.isEmpty()) {
					openForm.append("}\n");
					openForm.append("else\n");
					openForm.append("MessageDialog.openInformation(parentShell, ");
					openForm.append(i18n.getI18NMessage("msg_title_form_link", "Open detail form") + ", ");
					openForm.append(i18n.getI18NMessage("msg_info_form_link_no_sel", msg) + ");\n");
				}
			}
			else {
				if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX
						|| field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT)
					openForm.append("if(" + field.getName() + ".getSelectedItem() == null)\n");
				else if (field.getFieldType() == FormFieldTypeEnumeration.LOV) {
					String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
					lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);

					final JavaType pkType = pkAttr.getDomainAttribute().getJavaType();

					if (pkType.isString())
						openForm.append("if(" + lovIdValue + ".isEmpty())\n");
					else
						openForm.append("if(" + lovIdValue + " == " + pkType.getLocalVariableDefaultValue() + ")\n");
				}

				openForm.append("{\n");
				openForm.append("MessageDialog.openInformation(parentShell, ");
				openForm.append(i18n.getI18NMessage("msg_title_form_link", "Open detail form") + ", ");
				openForm.append(i18n.getI18NMessage("msg_info_form_link_no_sel", msg) + ");\n");
				openForm.append("return;\n");
				openForm.append("}\n\n");
				openForm.append("final var dlg = new " + readonlyForm.getName());
				openForm.append("(parentShell, ");

				if (field.getFieldType() == FormFieldTypeEnumeration.COMBOBOX
						|| field.getFieldType() == FormFieldTypeEnumeration.PROPOSAL_TEXT)
					openForm.append(field.getName() + ".getSelectedItem()" + "." + pkAttr.getModelGetterName());
				else if (field.getFieldType() == FormFieldTypeEnumeration.LOV) {
					String lovIdValue = field.getName().substring(COMBO_PREFIX.length());
					lovIdValue = LOV_PREFIX + lovIdValue.substring(0, 1).toUpperCase() + lovIdValue.substring(1);

					openForm.append(lovIdValue);
				}

				openForm.append(");\n");
				openForm.append("dlg.open();\n");
			}

			b.append(securityHelper.wrapSecurityCode(readonlyForm.getRoles(), openForm.toString()));
		}
		else
			b.append("// Appropriate form doesn't exist!\n");

		b.append("}\n");
		b.append("});\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	protected String addToolTipFragment() {
		final AbstractDomainAssociation assoc = field.getDTOAttribute().getAssociation();
		final DomainAttribute attr = field.getDTOAttribute().getDomainAttribute();

		if (attr != null && attr.getUserComment() != null && !attr.getUserComment().isEmpty())
			return field.getName() + ".setToolTipText(" + i18n.getI18N(attr) + ");\n";

		if (attr == null && assoc != null && assoc.getUserComment() != null && !assoc.getUserComment().isEmpty())
			return field.getName() + ".setToolTipText(" + i18n.getI18N(assoc) + ");\n";

		return "";
	}

	/**
	 * @return the generated content
	 */
	protected String fillEmptyColumns() {
		final var b = new StringBuilder();

		if (!field.fillEmptyGridColumn())
			return b.toString();

		b.append("\n// Add empty labels to fill the grid layout properly!\n");
		b.append("new Label(" + field.getPanel().getName() + ", SWT.NONE);\n");
		b.append("new Label(" + field.getPanel().getName() + ", SWT.NONE);\n\n");

		return b.toString();
	}

	/**
	 * @param gridDataName
	 * @param hasOneColumn
	 * @return the field layout
	 */
	private String addFieldLayout(String gridDataName, boolean hasOneColumn) {
		final var b = new StringBuilder();
		final FormFieldTypeEnumeration fieldType = field.getFieldType();
		final boolean allowSpan = field.isSpanCols() && field.getColIndex() == 1;

		b.append("\n");

		if (allowSpan) {
			if (hasOneColumn) {
				if (field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
					b.append("final var " + gridDataName + " = new GridData(SWT.FILL, SWT.FILL, true, true);\n");
				else
					b.append("final var " + gridDataName + " = new GridData(SWT.FILL, SWT.CENTER, true, false);\n");
			}
			else if (field.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
				b.append("final var " + gridDataName + " = new GridData(SWT.FILL, SWT.FILL, true, true, 3, 1);\n");
			else
				b.append("final var " + gridDataName + " = new GridData(SWT.FILL, SWT.CENTER, true, false, 3, 1);\n");
		}
		else {
			b.append("final var " + gridDataName + " = new GridData(SWT.LEFT, SWT.CENTER, true, false);\n");

			if (field.getWidth() > 0)
				b.append(gridDataName + ".widthHint = " + field.getWidth() + ";\n");
		}

		if (fieldType == FormFieldTypeEnumeration.MULTI_LINE_TEXT || fieldType == FormFieldTypeEnumeration.MULTI_LINE_LABEL)
			b.append(gridDataName + ".heightHint = 100;\n");

		b.append("\n");

		return b.toString();
	}

}
