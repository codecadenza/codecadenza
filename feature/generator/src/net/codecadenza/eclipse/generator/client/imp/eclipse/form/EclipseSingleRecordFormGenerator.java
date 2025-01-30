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
package net.codecadenza.eclipse.generator.client.imp.eclipse.form;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.GRID_DATA_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_PANEL;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.TABLE_PREFIX;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator;
import net.codecadenza.eclipse.generator.client.common.form.AbstractSingleRecordFormGenerator;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.file.EclipseFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.eclipse.form.field.EclipseFieldGeneratorFactory;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodTypeEnumeration;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldComparator;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormPanelComparator;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.ClientPlatformEnumeration;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for single-record forms of an Eclipse RCP/RAP application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EclipseSingleRecordFormGenerator extends AbstractSingleRecordFormGenerator {
	private static final String MSG_UPLOAD_FAILED = "Could not upload selected file! Message: ";
	private static final Pattern NEW_LINE_PATTERN = Pattern.compile("\n");

	private final EList<FormPanel> panelsOfFirstRow = new BasicEList<>();
	private final EList<FormPanel> panelsOfSecondRow = new BasicEList<>();
	private final HashMap<Integer, String> lazyLoadMap1 = new HashMap<>();
	private final HashMap<Integer, String> lazyLoadMap2 = new HashMap<>();
	private final HashMap<String, String> tableMap = new HashMap<>();
	private final HashMap<String, String> eagerLoadMap = new HashMap<>();
	private final RichClientI18NGenerator i18n;
	private final String dialogClassName;
	private boolean hasLazyLoadPanels;
	private boolean hasLazyLoad1;
	private boolean hasLazyLoad2;

	/**
	 * Constructor
	 * @param form
	 */
	public EclipseSingleRecordFormGenerator(Form form) {
		super(form);

		this.dialogClassName = form.isTitleArea() ? "TitleAreaDialog" : "Dialog";
		this.i18n = new RichClientI18NGenerator(project);

		// Save all panels in a hash map
		for (final FormPanel panel : form.getFormPanels())
			if (panel.getRowIndex() == 1) {
				this.panelsOfFirstRow.add(panel);

				if (panel.getBasePanel() != null) {
					final String tableName = TABLE_PREFIX + Integer.toString(panel.getRowIndex()) + Integer.toString(panel.getColIndex());

					this.tableMap.put(tableName, panel.getBasePanel().getName());

					if (panel.getColIndex() != 1) {
						this.hasLazyLoad1 = true;
						this.lazyLoadMap1.put(panel.getColIndex(), tableName);
					}
					else
						this.eagerLoadMap.put(tableName, "");
				}
			}
			else {
				this.panelsOfSecondRow.add(panel);

				if (panel.getBasePanel() != null) {
					final String tableName = TABLE_PREFIX + Integer.toString(panel.getRowIndex()) + Integer.toString(panel.getColIndex());

					this.tableMap.put(tableName, panel.getBasePanel().getName());

					if (panel.getColIndex() != 1) {
						this.hasLazyLoad2 = true;
						this.lazyLoadMap2.put(panel.getColIndex(), tableName);
					}
					else
						this.eagerLoadMap.put(tableName, "");
				}
			}

		if (hasLazyLoad1 || hasLazyLoad2)
			this.hasLazyLoadPanels = true;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addImports()
	 */
	@Override
	protected void addImports() {
		importStatic(project.getClientNamespace().toString() + "." + APP_I18N_PROVIDER_CLASS);
		importClass("org.eclipse.jface.dialogs.IDialogConstants");
		importClass("org.eclipse.jface.dialogs.MessageDialog");
		importClass("org.eclipse.swt.SWT");
		importPackage("org.eclipse.swt.layout");
		importPackage("org.eclipse.swt.widgets");
		importPackage("org.eclipse.swt.graphics");
		importPackage("net.codecadenza.runtime.richclient.eclipse.dialog");

		if (project.isBoundaryMode())
			importPackage(dto.getNamespace().toString());
		else
			importPackage(dto.getDomainObject().getNamespace().toString());

		form.getActions().forEach(action -> {
			if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) {
				// Add imports for further data transfer objects that are used when creating initial one-to-many association objects!
				for (final MethodParameter param : action.getBoundaryMethod().getMethodParameters())
					if (param.getType() instanceof final DTOBean dto) {
						String initDTOPackage;

						if (project.isBoundaryMode())
							initDTOPackage = dto.getNamespace().toString();
						else
							initDTOPackage = dto.getDomainObject().getNamespace().toString();

						importPackage(initDTOPackage);
					}

				// If we work directly with entities we must import packages of domain objects referenced by one-to-one associations!
				if (!project.isBoundaryMode())
					for (final DTOBeanAttribute attr : dto.getAttributes())
						if (attr.getAssociation() instanceof OneToOneAssociation)
							importPackage(attr.getAssociation().getTarget().getNamespace().toString());
			}
		});

		if (formType != FormTypeEnumeration.READONLY && !form.isTitleArea()) {
			importPackage("java.util");
			importPackage("net.codecadenza.runtime.richclient.validation");
		}

		if (form.isTitleArea())
			importClass("org.eclipse.jface.dialogs.TitleAreaDialog");
		else
			importClass("org.eclipse.jface.dialogs.Dialog");

		if (hasLazyLoadPanels) {
			importPackage("java.util");
			importPackage("org.eclipse.swt.events");
		}

		// Search for additional imports concerning actions
		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.DIRECT_UPLOAD || a.getType() == ActionType.INDIRECT_UPLOAD
					|| a.getType() == ActionType.DOWNLOAD) {
				importPackage("net.codecadenza.runtime.richclient.eclipse.image");
				importPackage("org.eclipse.swt.events");

				if (!project.isJavaSEApplication()) {
					importClass("org.eclipse.jface.dialogs.ProgressMonitorDialog");
					importPackage("net.codecadenza.runtime.richclient.eclipse.action");
				}

				new EclipseFileHandlingGenerator(this, a, i18n).addImports();
			}

		// Analyze the form to add all necessary imports
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() != null) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_PANEL);
				continue;
			}

			// Add imports for all form fields
			panel.getFields().forEach(field -> EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addImports());
		}

		// Check if further imports for data formatting are necessary
		form.getAllFormFields().forEach(field -> {
			boolean needsFormatter = false;

			final IFieldGenerator fieldGenerator = EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n);

			if (fieldGenerator.needsDateFormatter() || fieldGenerator.needsDateTimeFormatter()) {
				importPackage(PACK_JAVA_TIME_FORMAT);
				needsFormatter = true;
			}

			if (fieldGenerator.needsDecimalFormatter()) {
				importPackage("java.text");
				needsFormatter = true;
			}

			if (needsFormatter) {
				importPackage("net.codecadenza.runtime.richclient.format");

				if (project.hasRAPClient())
					importClass("net.codecadenza.runtime.richclient.eclipse.rap.services.FormatPreferencesManager");
			}
		});
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class ");
		b.append(form.getName());
		b.append(" extends ");
		b.append(dialogClassName);
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		final boolean addTransientModifier = project.getClientPlatform() == ClientPlatformEnumeration.RAP;
		boolean useDecimalFormatter = false;
		boolean useDateFormatter = false;
		boolean useDateTimeFormatter = false;

		// Check if we need the formatter objects
		for (final FormField field : form.getAllFormFields()) {
			if (!useDateFormatter)
				useDateFormatter = EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDateFormatter();

			if (!useDateTimeFormatter)
				useDateTimeFormatter = EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDateTimeFormatter();

			if (!useDecimalFormatter)
				useDecimalFormatter = EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDecimalFormatter();

			// We can stop further investigation if all formatter objects are necessary!
			if (useDecimalFormatter && useDateFormatter && useDateTimeFormatter)
				break;
		}

		addPrivateField("Shell", "parentShell").withFinalModifier().create();

		if (useDecimalFormatter || useDateFormatter || useDateTimeFormatter) {
			addPrivateField("FormatDTO", "userFormat").withDefaultValue("FormatPreferencesManager.getFormatDTO()").withFinalModifier()
					.create();

			if (useDecimalFormatter)
				addPrivateField("DecimalFormat", "decimalFormat").withDefaultValue("new DecimalFormat(userFormat.getDecimalFormat())")
						.withFinalModifier().create();

			if (useDateFormatter) {
				final var formatter = "DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(" + PACK_JAVA_TIME
						+ ".ZoneId.systemDefault())";

				addPrivateField("DateTimeFormatter", "dateFormat").withDefaultValue(formatter).setTransientModifier(addTransientModifier)
						.withFinalModifier().create();
			}

			if (useDateTimeFormatter) {
				final var formatter = "DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(" + PACK_JAVA_TIME
						+ ".ZoneId.systemDefault())";

				addPrivateField("DateTimeFormatter", "dateTimeFormat").withDefaultValue(formatter)
						.setTransientModifier(addTransientModifier).withFinalModifier().create();
			}
		}

		// Add all form field declarations
		form.getAllFormFields()
				.forEach(field -> EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addFieldDeclaration());

		// Add the ID attribute
		final DTOBeanAttribute pkDTOAttr = dto.getPKAttribute();

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			if (pkDTOAttr.getDomainAttribute().getJavaType().getNamespace() != null)
				importPackage(pkDTOAttr.getDomainAttribute().getJavaType().getNamespace().toString());

			addPrivateField(pkDTOAttr.getDomainAttribute().getJavaType().getName(), "id").withFinalModifier().create();
		}

		addPrivateField(dto.getModelClassName(), modelObjectName)
				.setTransientModifier(addTransientModifier && !project.isBoundaryMode())
				.withDefaultValue("new " + dto.getModelClassName() + "()").create();

		additionalDTOs.stream().forEach(
				addDTO -> addPrivateField(addDTO.getModelClassName(), INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName())
						.setTransientModifier(addTransientModifier && !project.isBoundaryMode()).create());

		if (hasLazyLoad1)
			addPrivateField("HashMap<Integer, Boolean>", "tableLoadMap1").withDefaultValue("new HashMap<>()").withFinalModifier()
					.create();

		if (hasLazyLoad2)
			addPrivateField("HashMap<Integer, Boolean>", "tableLoadMap2").withDefaultValue("new HashMap<>()").withFinalModifier()
					.create();

		tableMap.entrySet().forEach(entry -> addPrivateField(entry.getValue(), entry.getKey()).create());

		final var serviceSet = new HashSet<String>();

		// Add the declaration for all services
		final BoundaryBean boundary = project.getBoundaryByDomainObject(dto.getDomainObject());

		if (boundary != null) {
			serviceSet.add(boundary.getInterfaceName());

			new ServiceDeclarationGenerator(this, boundary).addField(true, addTransientModifier);
		}

		for (final FormField field : form.getAllFormFields()) {
			if (formType == FormTypeEnumeration.READONLY || !field.isVisible() || field.isReadonly())
				continue;

			// Some field types don't need a service!
			if (field.getDTOAttribute().getReferencedDTOBean() == null
					|| field.getDTOAttribute().getReferencedDTOBean().getDomainObject().equals(dto.getDomainObject())
					|| field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM
					|| field.getFieldType() == FormFieldTypeEnumeration.FORM_LINK
					|| field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_CLIENT
					|| field.getFieldType() == FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO
					|| (field.getFieldType() == FormFieldTypeEnumeration.LOV && project.isBoundaryMode()))
				continue;

			final DTOBean listDTO = field.getDTOAttribute().getReferencedDTOBean();
			final BoundaryBean listBoundary = project.getBoundaryByDomainObject(listDTO.getDomainObject());
			var interfaceName = "";

			if (listBoundary != null)
				interfaceName = listBoundary.getInterfaceName();

			// We should not declare a service twice!
			if (serviceSet.contains(interfaceName))
				continue;

			serviceSet.add(interfaceName);

			new ServiceDeclarationGenerator(this, listBoundary, boundary).addField(true, addTransientModifier);
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addConstructors()
	 */
	@Override
	protected void addConstructors() {
		final var b = new StringBuilder();
		final DTOBeanAttribute pkDTOAttribute = dto.getPKAttribute();
		var identifier = "";

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
			identifier = form.getName() + "(Shell parentShell, " + pkDTOAttribute.getDomainAttribute().getJavaType().getName() + " id)";
		else if (formType == FormTypeEnumeration.ADD) {
			for (final FormField field : form.getAllFormFields()) {
				if (field.getFieldType() != FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
					continue;

				final DTOBean dto = field.getDTOAttribute().getReferencedDTOBean();
				final DTOBeanAttribute pkAttr = dto.getPKAttribute();
				final String typeName = pkAttr.getDomainAttribute().getJavaType().getName();

				String attributeName = field.getName().substring(COMBO_PREFIX.length());
				attributeName = attributeName.substring(0, 1).toLowerCase() + attributeName.substring(1);

				identifier = form.getName() + "(Shell parentShell, " + typeName + " " + attributeName + ")";
				break;
			}
		}
		else
			identifier = form.getName() + "(Shell parentShell)";

		// Add the constructor
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param parentShell\n");

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			b.append(" * @param id the primary key of the form object\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + identifier + "\n");
			b.append("{\n");
			b.append("super(parentShell);\n\n");
			b.append("this.id = id;\n");
		}
		else if (formType == FormTypeEnumeration.ADD) {
			for (final FormField field : form.getAllFormFields()) {
				if (field.getFieldType() != FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
					continue;

				String attributeName = field.getName().substring(COMBO_PREFIX.length());
				attributeName = attributeName.substring(0, 1).toLowerCase() + attributeName.substring(1);

				b.append(" * @param " + attributeName + "\n");
				b.append(" */\n");
				b.append(getAnnotationForGeneratedElement());
				b.append("public " + identifier + "\n");
				b.append("{\n");
				b.append("super(parentShell);\n\n");
				b.append("this." + attributeName + " = " + attributeName + ";\n");
				break;
			}
		}
		else {
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + identifier + "\n");
			b.append("{\n");
			b.append("super(parentShell);\n");
		}

		b.append("\n");
		b.append(addInitializationOfAdditionalFields());
		b.append("this.parentShell = parentShell;\n");

		if (form.isModal() && form.isResizable())
			b.append("this.setShellStyle(super.getShellStyle() | SWT.RESIZE);\n");
		else if (!form.isModal() && form.isResizable())
			b.append("this.setShellStyle(SWT.CLOSE | SWT.RESIZE | SWT.MODELESS);\n");
		else if (!form.isModal() && !form.isResizable())
			b.append("this.setShellStyle(SWT.CLOSE | SWT.MODELESS);\n");

		b.append("}\n\n");

		addConstructor(identifier, b.toString());
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addMethods()
	 */
	@Override
	protected void addMethods() {
		final var fieldInitialization = new StringBuilder();

		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			form.getAllFormFields().forEach(field -> fieldInitialization
					.append(EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getDefaultInitializationFragment()));
		else
			form.getAllFormFields().forEach(field -> fieldInitialization
					.append(EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getCreateInitializationFragment()));

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.jface.dialogs." + dialogClassName + "#createDialogArea(org.eclipse.swt.widgets.Composite)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected Control createDialogArea(Composite parent)\n");
		b.append("{\n");
		b.append("final var container = (Composite) super.createDialogArea(parent);\n");

		if (form.isTitleArea()) {
			b.append("final long start = System.currentTimeMillis();\n\n");
			b.append("setTitle(" + i18n.getI18N(form) + ");\n");
		}

		b.append("\n");

		addDebugLog(b, "Initialize dialog");

		b.append("\n");

		if (!lazyLoadMap1.isEmpty() || !lazyLoadMap2.isEmpty()) {
			b.append("// Initialize lazy load map\n");

			for (final int colIndex : lazyLoadMap1.keySet())
				b.append("tableLoadMap1.put(" + (colIndex - 1) + ", false);\n");

			for (final int colIndex : lazyLoadMap2.keySet())
				b.append("tableLoadMap2.put(" + (colIndex - 1) + ", false);\n");

			b.append("\n");
		}

		if (!form.isTitleArea()) {
			b.append("final var gridLayoutContainer = new GridLayout();\n");
			b.append("gridLayoutContainer.verticalSpacing = 0;\n");
			b.append("gridLayoutContainer.horizontalSpacing = 0;\n\n");
			b.append("container.setLayout(gridLayoutContainer);\n");
		}

		b.append("final var composite = new Composite(container, SWT.NONE);\n");
		b.append("composite.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n");
		b.append("composite.setLayout(new GridLayout());\n\n");
		b.append(addToolBarItems());

		// Sort the form panels of the first row
		ECollections.sort(panelsOfFirstRow, new FormPanelComparator());

		// Check if the form needs a tab folder
		if (panelsOfFirstRow.size() > 1) {
			b.append("final var tabFolder1 = new TabFolder(composite, SWT.NONE);\n");

			if (!panelsOfSecondRow.isEmpty()) {
				if (!form.isTitleArea())
					b.append("tabFolder1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));\n");
				else {
					b.append("\n");
					b.append("final var gdTabFolder1 = new GridData(SWT.FILL, SWT.FILL, true, false);\n");
					b.append("tabFolder1.setLayoutData(gdTabFolder1);\n");
				}
			}
			else if (!form.isTitleArea())
				b.append("tabFolder1.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n");
			else {
				b.append("\n");
				b.append("final var gdTabFolder1 = new GridData(SWT.FILL, SWT.FILL, true, true);\n");
				b.append("tabFolder1.setLayoutData(gdTabFolder1);\n");
			}

			b.append("\n");

			if (!lazyLoadMap1.isEmpty()) {
				b.append("tabFolder1.addMouseListener(new MouseAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void mouseDown(MouseEvent e)\n");
				b.append("{\n");
				b.append("if(tableLoadMap1.get(tabFolder1.getSelectionIndex()) == null)\n");
				b.append("return;\n\n");
				b.append("if(!tableLoadMap1.get(tabFolder1.getSelectionIndex()))\n");
				b.append("{\n");

				boolean firstIndex = true;

				for (final Map.Entry<Integer, String> entry : lazyLoadMap1.entrySet()) {
					if (firstIndex)
						firstIndex = false;
					else
						b.append("else ");

					b.append("if(tabFolder1.getSelectionIndex() == " + (entry.getKey() - 1) + ")\n");
					b.append("{\n");
					b.append("tableLoadMap1.put(" + (entry.getKey() - 1) + ", true);\n");
					b.append(entry.getValue() + ".refreshData();\n");
					b.append("}\n");
				}

				b.append("}\n");
				b.append("}\n");
				b.append("});\n\n");
			}
		}

		int rowCount = 1;

		if (!panelsOfSecondRow.isEmpty())
			rowCount++;

		// Add the panels of the first row
		for (final FormPanel panel : panelsOfFirstRow)
			b.append(addPanelToForm(panel, panelsOfFirstRow.size(), rowCount, panelsOfFirstRow.size()));

		// Sort the panels of the second row
		ECollections.sort(panelsOfSecondRow, new FormPanelComparator());

		// Add a tab folder for the second row if the first row uses a tab folder
		if ((panelsOfFirstRow.size() > 1 && !panelsOfSecondRow.isEmpty()) || panelsOfSecondRow.size() > 1) {
			b.append("final var tabFolder2 = new TabFolder(composite, SWT.NONE);\n");

			if (!form.isTitleArea())
				b.append("tabFolder2.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n");
			else {
				b.append("\n");
				b.append("final var gdTabFolder2 = new GridData(SWT.FILL, SWT.FILL, true, true);\n");
				b.append("tabFolder2.setLayoutData(gdTabFolder2);\n");
			}

			b.append("\n");

			if (!lazyLoadMap2.isEmpty()) {
				b.append("tabFolder2.addMouseListener(new MouseAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see org.eclipse.swt.events.MouseAdapter#mouseDown(org.eclipse.swt.events.MouseEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void mouseDown(MouseEvent e)\n");
				b.append("{\n");
				b.append("if(tableLoadMap2.get(tabFolder2.getSelectionIndex()) == null)\n");
				b.append("return;\n\n");
				b.append("if(!tableLoadMap2.get(tabFolder2.getSelectionIndex()))\n");
				b.append("{\n");

				boolean firstIndex = true;

				for (final Map.Entry<Integer, String> entry : lazyLoadMap2.entrySet()) {
					if (firstIndex)
						firstIndex = false;
					else
						b.append("else ");

					b.append("if(tabFolder2.getSelectionIndex() == " + (entry.getKey() - 1) + ")\n");
					b.append("{\n");
					b.append("tableLoadMap2.put(" + (entry.getKey() - 1) + ", true);\n");
					b.append(entry.getValue() + ".refreshData();\n");
					b.append("}\n");
				}

				b.append("}\n");
				b.append("}\n");
				b.append("});\n\n");
			}
		}

		// Add the panels of the second row
		for (final FormPanel panel : panelsOfSecondRow)
			b.append(addPanelToForm(panel, panelsOfSecondRow.size(), rowCount, panelsOfFirstRow.size()));

		final boolean initDialog = !fieldInitialization.isEmpty() || formType == FormTypeEnumeration.UPDATE
				|| formType == FormTypeEnumeration.READONLY;

		if (initDialog)
			b.append("initDialog();\n\n");

		addDebugLog(b, "Dialog initialization finished");

		b.append("\n");

		if (form.isTitleArea()) {
			final var msg = "Dialog initialized successfully in {0} seconds";
			final var format = "String.format(\"%.2f\", (double)(end - start) / 1000)";

			b.append("final long end = System.currentTimeMillis();\n");
			b.append("setMessage(" + i18n.getI18NMessage("msg_status_init", msg, format) + ");\n\n");
		}

		b.append("return container;\n");
		b.append("}\n\n");

		addMethod("Control createDialogArea(Composite parent)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.jface.dialogs.Dialog#createButtonsForButtonBar(org.eclipse.swt.widgets.Composite)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected void createButtonsForButtonBar(Composite parent)\n");
		b.append("{\n");
		b.append("createButton(parent, IDialogConstants.OK_ID, " + i18n.getI18NMessage("cmd_ok", "OK") + ", false);\n");

		if (formType != FormTypeEnumeration.READONLY) {
			final var msgCancel = i18n.getI18NMessage("cmd_cancel", "Cancel");

			b.append("createButton(parent, IDialogConstants.CANCEL_ID, " + msgCancel + ", false);\n");
		}

		b.append("}\n");

		addMethod("void createButtonsForButtonBar(Composite parent)", b.toString());

		if (formType != FormTypeEnumeration.READONLY) {
			b = new StringBuilder();
			Form updateForm = null;

			if ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && form.isOpenEditAfterCreate()) {
				for (final Form f : project.getAllFormsOfProject())
					if (f.getFormType() == FormTypeEnumeration.UPDATE && form.getDomainObject().equals(f.getDomainObject())) {
						updateForm = f;
						break;
					}
			}

			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.jface.dialogs.Dialog#buttonPressed(int)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("protected void buttonPressed(int buttonId)\n");
			b.append("{\n");
			b.append("if(buttonId == IDialogConstants.OK_ID)\n");
			b.append("{\n");

			if (!form.isTitleArea()) {
				b.append("final Collection<FieldValidationBean> fieldErrors = validateUserInput();\n\n");
				b.append("if(!fieldErrors.isEmpty())\n");
				b.append("{\n");
				b.append("final var validationDialog = new FieldValidationDialog(parentShell, fieldErrors);\n");
				b.append("validationDialog.open();\n");
				b.append("return;\n");
				b.append("}\n\n");
			}
			else {
				b.append("if(!validateUserInput())\n");
				b.append("return;\n\n");
			}

			if (updateForm != null) {
				b.append("if(saveData())\n");
				b.append("{\n");
				b.append("close();\n");
				b.append("this.setReturnCode(" + dialogClassName + ".OK);\n\n");
				b.append("new " + updateForm.getName() + "(parentShell, " + dto.getDomainObject().getLowerCaseName() + ".");
				b.append(updateForm.getDTO().getPKAttribute().getModelGetterName() + ").open();\n");
				b.append("}\n\n");
			}
			else
				b.append("if(!saveData())\n");

			b.append("return;\n");
			b.append("}\n\n");
			b.append("super.buttonPressed(buttonId);\n");
			b.append("}\n\n");

			addMethod("void buttonPressed(int buttonId)", b.toString());
		}

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.jface.dialogs." + dialogClassName + "#getInitialSize()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected Point getInitialSize()\n");
		b.append("{\n");
		b.append("return DialogUtility.adaptSizeToSystemDPI(" + form.getWidth() + ", " + form.getHeight() + ");\n");
		b.append("}\n\n");

		addMethod("Point getInitialSize()", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see org.eclipse.jface.window.Window#configureShell(org.eclipse.swt.widgets.Shell)\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected void configureShell(Shell newShell)\n");
		b.append("{\n");
		b.append("super.configureShell(newShell);\n");
		b.append("newShell.setText(" + i18n.getI18N(form) + ");\n");
		b.append("}\n\n");

		addMethod("void configureShell(Shell newShell)", b.toString());

		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dto.getDomainObject());
		final var declarationGenerator = new ServiceDeclarationGenerator(this, boundaryBean);

		if (declarationGenerator.needsCloseStatement()) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see org.eclipse.jface.dialogs.Dialog#close()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public boolean close()\n");
			b.append("{\n");

			new ServiceDeclarationGenerator(this, boundaryBean, b).addCloseStatement();

			b.append("return super.close();\n");
			b.append("}\n\n");

			addMethod("boolean close()", b.toString());
		}

		if (initDialog)
			addMethod("void initDialog()", createInitDialogMethod(fieldInitialization.toString()));

		if (formType != FormTypeEnumeration.READONLY) {
			addMethod("boolean validateUserInput()", createValidationMethod());

			addMethod("boolean saveData()", createFormDataSaveMethod());
		}

		i18n.save();
	}

	/**
	 * Create the method to load all data and to initialize all form fields
	 * @param fieldInitializationFragement
	 * @return the generated content
	 */
	private String createInitDialogMethod(String fieldInitializationFragement) {
		final var b = new StringBuilder();
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dto.getDomainObject());

		if (boundaryBean == null)
			throw new IllegalStateException(
					"The boundary bean for domain object '" + dto.getDomainObject().getName() + "' could not be found!");

		b.append("/**\n");
		b.append(" * Initialize the dialog. The dialog will be closed if the initialization fails!\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private void initDialog()\n");
		b.append("{\n");
		b.append("parentShell.setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_WAIT));\n\n");
		b.append("try\n");
		b.append("{\n");

		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY) {
			addDebugLog(b, "Fetch data for object with id '{}'", "id");

			b.append("\n");
			b.append(modelObjectName + " = ");

			if (project.isBoundaryMode()) {
				final BoundaryMethod method = boundaryBean.getBoundaryMethodByReturnType(dto, BoundaryMethodTypeEnumeration.FIND_BY_ID);

				new ServiceInvocationGenerator(method, b).addInvocation("id");
			}
			else {
				final RepositoryMethod method = boundaryBean.getRepository()
						.getMethodByType(RepositoryMethodTypeEnumeration.FIND_EXISTING);

				new ServiceInvocationGenerator(method, b).addInvocation("id", "true");
			}

			if (!fieldInitializationFragement.isEmpty())
				b.append("\n");
		}

		b.append(fieldInitializationFragement);

		if (!eagerLoadMap.isEmpty()) {
			b.append("\n");
			b.append("// Load data of grid panels that are displayed by default\n");

			for (final String tableName : eagerLoadMap.keySet())
				b.append(tableName + ".refreshData();\n");
		}

		b.append("\n");
		b.append("parentShell.setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_ARROW));\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			addErrorLog(b, "Error while initializing dialog for object with id '{}'!", "ex", "id");
		else
			addErrorLog(b, "Error while initializing dialog!", "ex");

		b.append("\n");
		b.append("parentShell.setCursor(Display.getDefault().getSystemCursor(SWT.CURSOR_ARROW));\n");
		b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_form_init", "Initialize dialog") + ", ");
		b.append(i18n.getI18NMessage("msg_err_init_dialog", "Error while initializing dialog! Message: "));
		b.append(" + ex.getMessage());\n\n");
		b.append("close();\n");
		b.append("}\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * Create the method to validate user input
	 * @return the generated content
	 */
	private String createValidationMethod() {
		final var b = new StringBuilder();
		final var val = new StringBuilder();
		final var msgMissingFile = "File that refers to field {0} hasn't been selected yet!";

		b.append("/**\n");
		b.append(" * Check user input\n");

		if (form.isTitleArea())
			b.append(" * @return true if validation was finished successfully!\n");
		else
			b.append(" * @return a collection of field validation objects!\n");

		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());

		if (form.isTitleArea())
			b.append("private boolean validateUserInput()\n");
		else
			b.append("private Collection<FieldValidationBean> validateUserInput()\n");

		b.append("{\n");

		// If a form contains an action of type 'DIRECT_UPLOAD' we have to add a check if the corresponding field isn't nullable!
		for (final FormAction action : form.getActions()) {
			if (action.getType() != ActionType.DIRECT_UPLOAD)
				continue;

			final DTOBeanAttribute dataAttribute = dto.getAttributes().stream()
					.filter(attribute -> attribute.getDomainAttribute() != null
							&& attribute.getDomainAttribute().equals(action.getBoundaryMethod().getDomainAttribute()))
					.findFirst().orElse(null);

			if (dataAttribute == null)
				throw new IllegalStateException("The DTO attribute for form action '" + action.getName() + "' could not be found!");

			// Test if the file upload is mandatory!
			if (dataAttribute.getDomainAttribute().isLob()
					&& dataAttribute.getDomainAttribute().getDomainAttributeValidator().isNullable())
				continue;

			if (!dataAttribute.getDomainAttribute().isLob() && dataAttribute.getDomainAttribute().getMinFieldLength().isEmpty())
				continue;

			val.append("// Check if user has already uploaded a file for respective field!\n");
			val.append("if(" + modelObjectName + "." + dataAttribute.getModelGetterName() + " == null");

			if (project.isBoundaryMode())
				val.append(" || " + modelObjectName + "." + dataAttribute.getModelGetterName() + ".isEmpty()");

			val.append(")\n");
			val.append("{\n");
			val.append("final String " + FIELD_LABEL_VALIDATION + " = " + i18n.getI18N(dataAttribute) + ";\n");

			if (!form.isTitleArea()) {
				val.append("fieldErrors.add(new FieldValidationBean(" + FIELD_LABEL_VALIDATION + ", ");
				val.append(i18n.getI18NMessage("msg_err_missing_file_ref", msgMissingFile, FIELD_LABEL_VALIDATION) + "));\n");
			}
			else {
				final var msgMissingFileRef = i18n.getI18NMessage("msg_err_missing_file_ref", msgMissingFile, FIELD_LABEL_VALIDATION);

				val.append("setErrorMessage(" + msgMissingFileRef + ");\n");
				val.append("return false;\n");
			}

			val.append("}\n\n");
		}

		// Validate the input of all form fields
		form.getAllFormFields().forEach(field -> val
				.append(EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getValidationFragment(form.isTitleArea())));

		if (!val.toString().isEmpty()) {
			for (final FormField field : form.getAllFormFields()) {
				final FormFieldTypeEnumeration fieldType = field.getFieldType();

				if (fieldType == FormFieldTypeEnumeration.MULTI_LINE_TEXT || fieldType == FormFieldTypeEnumeration.SIMPLE_TEXT
						|| field.getFieldType() == FormFieldTypeEnumeration.DATE || fieldType == FormFieldTypeEnumeration.DATE_TIME) {
					final String validationFragment = EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n)
							.getValidationFragment(form.isTitleArea());

					if (!validationFragment.isEmpty()) {
						b.append("String inputToCheck = \"\";\n");
						break;
					}
				}
			}

			if (!form.isTitleArea())
				b.append("final var fieldErrors = new ArrayList<FieldValidationBean>();\n\n");
			else
				b.append("setErrorMessage(null);\n\n");

			b.append(val);
		}

		if (!form.isTitleArea()) {
			if (!val.toString().isEmpty())
				b.append("return fieldErrors;\n");
			else
				b.append("return new ArrayList<>();\n");
		}
		else
			b.append("return true;\n");

		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * @return the generated content
	 */
	private String createUploadFragment() {
		final var b = new StringBuilder();

		for (final FormAction a : form.getActions()) {
			if (a.getType() != ActionType.DIRECT_UPLOAD)
				continue;

			for (final DTOBeanAttribute dtoAttr : dto.getAttributes()) {
				if (dtoAttr.getDomainAttribute() == null)
					continue;

				if (!dtoAttr.getDomainAttribute().equals(a.getBoundaryMethod().getDomainAttribute()))
					continue;

				boolean isMandatory = true;

				if (dtoAttr.getDomainAttribute().isLob() && dtoAttr.getDomainAttribute().getDomainAttributeValidator().isNullable())
					isMandatory = false;

				if (!dtoAttr.getDomainAttribute().isLob() && dtoAttr.getDomainAttribute().getMinFieldLength().isEmpty())
					isMandatory = false;

				if (!isMandatory) {
					b.append("if(" + modelObjectName + "." + dtoAttr.getModelGetterName() + "!= null && !" + modelObjectName + ".");
					b.append(dtoAttr.getModelGetterName() + ".isEmpty())\n");
					b.append("{\n");
				}
				else
					b.append("\n");

				b.append("try\n");
				b.append("{\n");

				addDebugLog(b, "Upload file");

				b.append("\n");
				b.append("// Upload data to server\n");
				b.append("final var uploadOperation = new FileUploadOperation(");
				b.append(modelObjectName + "." + dtoAttr.getModelGetterName() + ");\n\n");
				b.append("new ProgressMonitorDialog(parentShell).run(true, false, uploadOperation);\n");
				b.append(modelObjectName + "." + dtoAttr.getModelSetterName() + "(uploadOperation.getServerPath());\n");
				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");
				b.append("if(ex instanceof InterruptedException)\n");
				b.append("Thread.currentThread().interrupt();\n\n");

				addErrorLog(b, "Error while performing file upload operation!", "ex");

				b.append("\n");

				if (!form.isTitleArea()) {
					final var msgUploadError = i18n.getI18NMessage("msg_err_upload", "Could not upload selected file! Message: ");

					b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_upload", "Upload file") + ", ");
					b.append(msgUploadError + " + ex.getMessage());\n");
				}
				else
					b.append("setErrorMessage(" + i18n.getI18NMessage("msg_err_upload", MSG_UPLOAD_FAILED) + " + ex.getMessage());\n");

				b.append("return false;\n");
				b.append("}\n");

				if (!isMandatory)
					b.append("}\n\n");
				else
					b.append("\n");

				break;
			}
		}

		return b.toString();
	}

	/**
	 * Create the method to save form data
	 * @return the generated content
	 */
	private String createFormDataSaveMethod() {
		final var b = new StringBuilder();
		boolean firstFragment = true;

		b.append("/**\n");
		b.append(" * Save form data\n");
		b.append(" * @return true if data was saved successfully!\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public boolean saveData()\n");
		b.append("{\n");

		addDebugLog(b, "Perform save operation");

		b.append("\n");

		// Add fragments for saving data for all form fields
		for (final FormField field : form.getAllFormFields()) {
			final DTOBean fieldDTO = field.getDTOAttribute().getDTOBean();
			String objectName = modelObjectName;

			if (!fieldDTO.equals(dto))
				objectName = INIT_MODEL_OBJ_NAME_PREFIX + fieldDTO.getDomainObject().getName();

			String fragment = EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getSaveDataFragment(objectName);

			// The first line shouldn't be empty!
			if (!fragment.isEmpty() && firstFragment) {
				if (fragment.startsWith("\n"))
					fragment = NEW_LINE_PATTERN.matcher(fragment).replaceFirst("");

				firstFragment = false;
			}

			b.append(fragment);
		}

		if (!project.isJavaSEApplication() && (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE))
			b.append(createUploadFragment());

		b.append("\ntry\n");
		b.append("{\n");

		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.CREATE || a.getType() == ActionType.UPDATE) {
				final String resultObjName = dto.getDomainObject().getLowerCaseName();

				b.append("// Save data\n");

				if (!a.getBoundaryMethod().getReturnType().isVoid())
					b.append(resultObjName + " = ");

				new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation(getSaveInvocationParameters());

				b.append("\n");
				b.append("return true;\n");
				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");

				addErrorLog(b, "Error while performing save operation!", "ex");

				b.append("\n");
				b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_save", "Save operation") + ", ");
				b.append(i18n.getI18NMessage("msg_err_save", "Error while performing save operation! Message: "));
				b.append(" + ex.getMessage());\n");
				b.append("}\n\n");
				b.append("return false;\n");
				b.append("}\n\n");
				break;
			}

		return b.toString();
	}

	/**
	 * @param domainObject
	 * @return the generated content
	 */
	private String addDocumentFieldLogic(DomainObject domainObject) {
		final var b = new StringBuilder();

		// Check if fields with appropriate tagging exist that should be filled automatically!
		for (final FormField f : form.getAllFormFields())
			if (f.getDTOAttribute().getDomainAttribute() != null
					&& f.getDTOAttribute().getDomainAttribute().getDomainObject().equals(domainObject)) {
				final DomainAttribute attr = f.getDTOAttribute().getDomainAttribute();

				if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_NAME) {
					if (f.isVisible())
						b.append(f.getName() + ".setText(new File(path).getName());\n");

					b.append(dto.getDomainObject().getLowerCaseName() + "." + f.getDTOAttribute().getModelSetterName());
					b.append("(new File(path).getName());\n");
				}

				if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_SIZE) {
					if (f.isVisible())
						b.append(f.getName() + ".setText(ByteConverter.convert(new File(path).length()));\n");

					b.append(dto.getDomainObject().getLowerCaseName() + "." + f.getDTOAttribute().getModelSetterName());

					if (attr.getJavaType().isInteger())
						b.append("((int) new File(path).length());\n");
					else
						b.append("(new File(path).length());\n");
				}
			}

		return b.toString();
	}

	/**
	 * Add the toolbar and its items
	 * @return the generated content
	 */
	private String addToolBarItems() {
		final var b = new StringBuilder();
		final boolean addToolbar = form.getActions().stream().anyMatch(a -> a.getType() == ActionType.DOWNLOAD
				|| a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD);

		if (addToolbar)
			b.append("final var toolBar = new ToolBar(composite, SWT.RIGHT);\n\n");

		for (final FormAction a : form.getActions()) {
			if (a.getType() == ActionType.DOWNLOAD) {
				final var toolItemName = a.getName() + "Item";

				b.append("final var " + toolItemName + " = new ToolItem(toolBar, SWT.NONE);\n");
				b.append(toolItemName + ".setText(" + i18n.getI18NMessage("action_name_download", "Download") + ");\n");
				b.append(toolItemName + ".setToolTipText(" + i18n.getI18N(a) + ");\n");
				b.append(toolItemName + ".setImage(ImageCache.getImage(\"download.png\"));\n\n");
				b.append(toolItemName + ".addSelectionListener(new SelectionAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void widgetSelected(SelectionEvent e)\n");
				b.append("{\n");
				b.append(new EclipseFileHandlingGenerator(this, a, i18n).createDownloadFragment("id"));
				b.append("}\n");
				b.append("});\n\n");
			}
			else if (a.getType() == ActionType.INDIRECT_UPLOAD) {
				final var toolItemName = a.getName() + "Item";
				final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();
				final String maxSize = uploadAttr.getMaxFileSize();
				final var convertedFileSize = "ByteConverter.convert(" + maxSize + ")";
				final var msgSize = i18n.getI18NMessage("msg_err_upload_size", "Selected file exceeds size limit of {0}!",
						convertedFileSize);

				b.append("final var " + toolItemName + " = new ToolItem(toolBar, SWT.NONE);\n");
				b.append(toolItemName + ".setText(" + i18n.getI18NMessage("action_name_upload", "Upload") + ");\n");
				b.append(toolItemName + ".setToolTipText(" + i18n.getI18N(a) + ");\n");
				b.append(toolItemName + ".setImage(ImageCache.getImage(\"upload.png\"));\n\n");
				b.append(toolItemName + ".addSelectionListener(new SelectionAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void widgetSelected(SelectionEvent e)\n");
				b.append("{\n");
				b.append("final var dlg = new FileDialog(parentShell);\n");
				b.append("final String path = dlg.open();\n\n");
				b.append("if(path == null)\n");
				b.append("return;\n\n");
				b.append("// Validate file length\n");
				b.append("if(new File(path).length() > " + maxSize + ")\n");
				b.append("{\n");

				if (!form.isTitleArea()) {
					b.append("MessageDialog.openInformation(parentShell, " + i18n.getI18NMessage("msg_title_upload", "Upload file") + ", ");
					b.append(msgSize + ");\n");
				}
				else
					b.append("setErrorMessage(" + msgSize + ");\n");

				b.append("return;\n");
				b.append("}\n\n");

				if (form.isTitleArea()) {
					b.append("// Clear error message\n");
					b.append("setErrorMessage(null);\n\n");
				}

				addDebugLog(b, "Upload file");

				b.append("\n");
				b.append("try\n");
				b.append("{\n");

				if (!project.isJavaSEApplication()) {
					b.append("// Upload data to server\n");
					b.append("final var uploadOperation = new FileUploadOperation(path);\n\n");
					b.append("new ProgressMonitorDialog(parentShell).run(true, false, uploadOperation);\n");
					b.append("final String serverPath = uploadOperation.getServerPath();\n");

					new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation("id", "serverPath");
				}
				else
					new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation("id", "path");

				b.append("\n");

				// Set fields that belong to documents
				b.append(addDocumentFieldLogic(a.getBoundaryMethod().getDomainAttribute().getDomainObject()));

				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");

				if (!project.isJavaSEApplication()) {
					b.append("if(ex instanceof InterruptedException)\n");
					b.append("Thread.currentThread().interrupt();\n\n");
				}

				addErrorLog(b, "Error while performing file upload operation!", "ex");

				b.append("\n");

				if (!form.isTitleArea()) {
					b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_upload", "Upload file") + ", ");
					b.append(i18n.getI18NMessage("msg_err_upload", MSG_UPLOAD_FAILED) + " + ex.getMessage());\n");
				}
				else
					b.append("setErrorMessage(" + i18n.getI18NMessage("msg_err_upload", MSG_UPLOAD_FAILED) + " + ex.getMessage());\n");

				b.append("}\n");
				b.append("}\n");
				b.append("});\n\n");
			}
			else if (a.getType() == ActionType.DIRECT_UPLOAD) {
				final var toolItemName = a.getName() + "Item";
				final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();
				final String maxSize = uploadAttr.getMaxFileSize();
				final var convertedFileSize = "ByteConverter.convert(" + maxSize + ")";
				final var msgSize = i18n.getI18NMessage("msg_err_upload_size", "Selected file exceeds size limit of {0}!",
						convertedFileSize);

				b.append("final var " + toolItemName + " = new ToolItem(toolBar, SWT.NONE);\n");
				b.append(toolItemName + ".setText(" + i18n.getI18NMessage("action_name_browse", "Browse") + ");\n");
				b.append(toolItemName + ".setToolTipText(" + i18n.getI18N(a) + ");\n");
				b.append(toolItemName + ".setImage(ImageCache.getImage(\"upload.png\"));\n\n");
				b.append(toolItemName + ".addSelectionListener(new SelectionAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void widgetSelected(SelectionEvent e)\n");
				b.append("{\n");
				b.append("final var dlg = new FileDialog(parentShell);\n");
				b.append("final String path = dlg.open();\n\n");
				b.append("if(path == null)\n");
				b.append("return;\n\n");
				b.append("// Validate file length\n");
				b.append("if(new File(path).length() > " + maxSize + ")\n");
				b.append("{\n");

				if (!form.isTitleArea()) {
					b.append("MessageDialog.openInformation(parentShell, " + i18n.getI18NMessage("msg_title_upload", "Upload file") + ", ");
					b.append(msgSize + ");\n");
				}
				else
					b.append("setErrorMessage(" + msgSize + ");\n");

				b.append("return;\n");
				b.append("}\n\n");

				if (form.isTitleArea()) {
					b.append("// Clear error message\n");
					b.append("setErrorMessage(null);\n\n");
				}

				addDebugLog(b, "Upload file");

				b.append("\n");
				b.append("try\n");
				b.append("{\n");

				var setter = "";

				for (final DTOBeanAttribute dtoAttr : dto.getAttributes()) {
					if (dtoAttr.getDomainAttribute() == null)
						continue;

					if (dtoAttr.getDomainAttribute().equals(a.getBoundaryMethod().getDomainAttribute())) {
						setter = dtoAttr.getModelSetterName();
						break;
					}
				}

				b.append(modelObjectName + "." + setter + "(");

				if (project.isBoundaryMode() || !uploadAttr.isLob())
					b.append("path");
				else if (uploadAttr.getJavaType().isType(JavaType.BYTE_ARRAY))
					b.append("FileUtil.getBytesFromFile(new File(path))");
				else
					b.append("FileUtil.convertToByteArray(FileUtil.getBytesFromFile(new File(path)))");

				b.append(");\n");

				// Set fields that belong to documents
				b.append(addDocumentFieldLogic(a.getBoundaryMethod().getDomainAttribute().getDomainObject()));

				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");

				addErrorLog(b, "Error while performing file upload operation!", "ex");

				b.append("\n");

				if (!form.isTitleArea()) {
					b.append("MessageDialog.openError(parentShell, " + i18n.getI18NMessage("msg_title_upload", "Upload file") + ", ");
					b.append(i18n.getI18NMessage("msg_err_upload", MSG_UPLOAD_FAILED) + " + ex.getMessage());\n");
				}
				else
					b.append("setErrorMessage(" + i18n.getI18NMessage("msg_err_upload", MSG_UPLOAD_FAILED) + " + ex.getMessage());\n");

				b.append("}\n");
				b.append("}\n");
				b.append("});\n\n");
			}
		}

		return b.toString();
	}

	/**
	 * Add the panel to the form
	 * @param panel
	 * @param rowPanelCount
	 * @param rowCount
	 * @param columnsOfFirstRow
	 * @return the generated content
	 */
	private String addPanelToForm(FormPanel panel, int rowPanelCount, int rowCount, int columnsOfFirstRow) {
		final var b = new StringBuilder();
		boolean hasGrid = false;
		boolean hasOneColumn = true;

		if (panel.getBasePanel() != null)
			hasGrid = true;

		boolean addToTabFolder = false;

		if (panel.getRowIndex() == 1)
			if (rowPanelCount == 1)
				addToTabFolder = false;
			else
				addToTabFolder = true;
		else if (rowPanelCount > 1 || columnsOfFirstRow > 1)
			addToTabFolder = true;

		if (!addToTabFolder) {
			if (panel.isDrawBorder()) {
				b.append("final var " + panel.getName() + " = new Group(composite, SWT.NONE);\n");
				b.append(panel.getName() + ".setText(" + i18n.getI18N(panel) + ");\n");
			}
			else
				b.append("final var " + panel.getName() + " = new Composite(composite, SWT.NONE);\n");

			if (!hasGrid) {
				// Check if the panel uses more than one column
				for (final FormField f : panel.getFields())
					if (f.getColIndex() == 2) {
						hasOneColumn = false;
						break;
					}

				if (hasOneColumn)
					b.append(panel.getName() + ".setLayout(new GridLayout(2, false));\n");
				else
					b.append(panel.getName() + ".setLayout(new GridLayout(4, false));\n");

				if (rowCount == 1 || panel.getRowIndex() == 2)
					b.append(panel.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n");
				else
					b.append(panel.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, false));\n");

				b.append("\n");
			}
			else {
				b.append(panel.getName() + ".setLayout(new GridLayout());\n");
				b.append(panel.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n\n");

				final String tableName = TABLE_PREFIX + Integer.toString(panel.getRowIndex()) + Integer.toString(panel.getColIndex());
				final String gridDataName = GRID_DATA_PREFIX + tableName.substring(0, 1).toUpperCase() + tableName.substring(1);
				final AbstractDomainAssociation association = panel.getBasePanel().getAssociation();

				if (association instanceof ManyToManyAssociation
						|| association instanceof final OneToManyAssociation otm && !otm.isBidirectional())
					b.append(tableName + " = new " + panel.getBasePanel().getName() + "(" + panel.getName() + ", parentShell, id);\n");
				else if (panel.getForm().getFormType() == FormTypeEnumeration.READONLY) {
					b.append(tableName + " = new " + panel.getBasePanel().getName() + "(" + panel.getName());
					b.append(", parentShell, id, true);\n");
				}
				else {
					b.append(tableName + " = new " + panel.getBasePanel().getName() + "(" + panel.getName());
					b.append(", parentShell, id, false);\n");
				}

				b.append("\n");
				b.append("final var " + gridDataName + " = new GridData(SWT.FILL, SWT.FILL, true, true);\n");
				b.append(tableName + ".setLayoutData(" + gridDataName + ");\n\n");
			}
		}
		else {
			final var tabItemName = "tabItem" + Integer.toString(panel.getRowIndex()) + Integer.toString(panel.getColIndex());

			b.append("final var " + tabItemName + " = new TabItem(tabFolder" + panel.getRowIndex() + ", SWT.NONE);\n");
			b.append(tabItemName + ".setText(" + i18n.getI18N(panel) + ");\n\n");
			b.append("final var " + panel.getName() + " = new Composite(tabFolder" + panel.getRowIndex() + ", SWT.NONE);\n");
			b.append(panel.getName() + ".setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));\n");

			if (!hasGrid) {
				// Check if the panel uses more than one column
				for (final FormField f : panel.getFields())
					if (f.getColIndex() == 2) {
						hasOneColumn = false;
						break;
					}

				if (hasOneColumn)
					b.append(panel.getName() + ".setLayout(new GridLayout(2, false));\n");
				else
					b.append(panel.getName() + ".setLayout(new GridLayout(4, false));\n");

				b.append("\n");
			}
			else {
				b.append(panel.getName() + ".setLayout(new GridLayout());\n\n");

				final String tableName = TABLE_PREFIX + Integer.toString(panel.getRowIndex()) + Integer.toString(panel.getColIndex());
				final String gridDataName = GRID_DATA_PREFIX + tableName.substring(0, 1).toUpperCase() + tableName.substring(1);
				final AbstractDomainAssociation association = panel.getBasePanel().getAssociation();

				if (association instanceof ManyToManyAssociation
						|| association instanceof final OneToManyAssociation otm && !otm.isBidirectional())
					b.append(tableName + " = new " + panel.getBasePanel().getName() + "(" + panel.getName() + ", parentShell, id);\n");
				else if (panel.getForm().getFormType() == FormTypeEnumeration.READONLY) {
					b.append(tableName + " = new " + panel.getBasePanel().getName() + "(" + panel.getName());
					b.append(", parentShell, id, true);\n");
				}
				else {
					b.append(tableName + " = new " + panel.getBasePanel().getName() + "(" + panel.getName());
					b.append(", parentShell, id, false);\n");
				}

				b.append("\n");
				b.append("final var " + gridDataName + " = new GridData(SWT.FILL, SWT.FILL, true, true);\n");
				b.append(tableName + ".setLayoutData(" + gridDataName + ");\n\n");
			}

			b.append(tabItemName + ".setControl(" + panel.getName() + ");\n\n");
		}

		if (!hasGrid) {
			// Sort the form fields of this panel
			ECollections.sort(panel.getFields(), new FormFieldComparator());

			// Add the form fields to the panel
			for (final FormField field : panel.getFields())
				b.append(EclipseFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getFieldDefinitionFragment(hasOneColumn));
		}

		return b.toString();
	}

}
