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
package net.codecadenza.eclipse.generator.client.imp.javafx.form;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_PANEL;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.TABLE_PREFIX;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import net.codecadenza.eclipse.generator.client.common.form.AbstractSingleRecordFormGenerator;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.file.JavaFXFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.javafx.form.field.JavaFXFieldGeneratorFactory;
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
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodTypeEnumeration;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;
import org.eclipse.emf.common.util.EList;

/**
 * <p>
 * Generator for single-record forms of a JavaFX application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class JavaFXSingleRecordFormGenerator extends AbstractSingleRecordFormGenerator {
	private final EList<FormPanel> panelsOfFirstRow = new BasicEList<>();
	private final EList<FormPanel> panelsOfSecondRow = new BasicEList<>();
	private final HashMap<Integer, String> lazyLoadMap1 = new HashMap<>();
	private final HashMap<Integer, String> lazyLoadMap2 = new HashMap<>();
	private final HashMap<String, String> tableMap = new HashMap<>();
	private final HashMap<String, String> eagerLoadMap = new HashMap<>();
	private final RichClientI18NGenerator i18n;
	private boolean hasLazyLoadPanels;
	private boolean hasLazyLoad1;
	private boolean hasLazyLoad2;

	/**
	 * Constructor
	 * @param form
	 */
	public JavaFXSingleRecordFormGenerator(Form form) {
		super(form);

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
		importPackage("javafx.scene.layout");
		importPackage("javafx.stage");
		importPackage("javafx.scene");
		importPackage("net.codecadenza.runtime.richclient.javafx.dialog");

		if (form.isTitleArea())
			importPackage("net.codecadenza.runtime.richclient.javafx.image");

		if (project.isBoundaryMode()) {
			// Add import of the data transfer object that is mapped to this form
			importPackage(dto.getNamespace().toString());
		}
		else
			importPackage(dto.getDomainObject().getNamespace().toString());

		if (panelsOfFirstRow.size() > 1 || panelsOfSecondRow.size() > 1)
			importPackage("javafx.scene.control");

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

		if (hasLazyLoadPanels)
			importPackage("java.util");

		// Search for additional imports concerning actions
		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.DIRECT_UPLOAD || a.getType() == ActionType.INDIRECT_UPLOAD) {
				importPackage("java.io");
				importPackage("javafx.scene.control");
				importPackage("net.codecadenza.runtime.file");
			}
			else if (a.getType() == ActionType.DOWNLOAD) {
				importPackage("javafx.scene.control");
				importPackage("java.io");

				if (project.isJavaSEApplication())
					importPackage("net.codecadenza.runtime.file");
			}

		// Analyze the form to add all necessary imports
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() != null) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_PANEL);
				continue;
			}

			// Add imports for all form fields
			panel.getFields().forEach(field -> JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addImports());
		}
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addClassDeclaration(java.lang.StringBuilder)
	 */
	@Override
	protected void addClassDeclaration(StringBuilder b) {
		b.append("public class ");
		b.append(form.getName());

		if (form.isTitleArea())
			b.append(" extends TitleAreaDialog");
		else
			b.append(" extends AbstractBaseDialog");
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.generator.common.AbstractJavaSourceGenerator#addFields()
	 */
	@Override
	protected void addFields() {
		boolean useDecimalFormatter = false;
		boolean useDateFormatter = false;
		boolean useDateTimeFormatter = false;

		// Check if we need the formatter objects
		for (final FormField field : form.getAllFormFields()) {
			if (!useDateFormatter)
				useDateFormatter = JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDateFormatter();

			if (!useDateTimeFormatter)
				useDateTimeFormatter = JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDateTimeFormatter();

			if (!useDecimalFormatter)
				useDecimalFormatter = JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDecimalFormatter();

			// We can stop further investigation if all formatter objects are necessary!
			if (useDecimalFormatter && useDateFormatter && useDateTimeFormatter)
				break;
		}

		if (useDecimalFormatter || useDateFormatter || useDateTimeFormatter) {
			addPrivateField("FormatDTO", "userFormat").withDefaultValue("FormatPreferencesManager.getFormatDTO()").withFinalModifier()
					.create();

			if (useDecimalFormatter)
				addPrivateField("DecimalFormat", "decimalFormat").withDefaultValue("new DecimalFormat(userFormat.getDecimalFormat())")
						.withFinalModifier().create();

			if (useDateFormatter) {
				final var formatter = "DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(" + PACK_JAVA_TIME
						+ ".ZoneId.systemDefault())";

				addPrivateField("DateTimeFormatter", "dateFormat").withDefaultValue(formatter).withFinalModifier().create();
			}

			if (useDateTimeFormatter) {
				final var formatter = "DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(" + PACK_JAVA_TIME
						+ ".ZoneId.systemDefault())";

				addPrivateField("DateTimeFormatter", "dateTimeFormat").withDefaultValue(formatter).withFinalModifier().create();
			}
		}

		// Add all form field declarations
		form.getAllFormFields()
				.forEach(field -> JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addFieldDeclaration());

		// Add the ID attribute
		final DTOBeanAttribute pkDTOAttr = dto.getPKAttribute();

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			if (pkDTOAttr.getDomainAttribute().getJavaType().getNamespace() != null)
				importPackage(pkDTOAttr.getDomainAttribute().getJavaType().getNamespace().toString());

			addPrivateField(pkDTOAttr.getDomainAttribute().getJavaType().getName(), "id").withFinalModifier().create();
		}

		// Add the declaration for the model object
		addPrivateField(dto.getModelClassName(), modelObjectName).withDefaultValue("new " + dto.getModelClassName() + "()").create();

		additionalDTOs.stream().forEach(
				addDTO -> addPrivateField(addDTO.getModelClassName(), INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName())
						.create());

		if (hasLazyLoad1)
			addPrivateField("HashMap<Integer, Boolean>", "tableLoadMap1").withDefaultValue("new HashMap<>()").withFinalModifier()
					.create();

		if (hasLazyLoad2)
			addPrivateField("HashMap<Integer, Boolean>", "tableLoadMap2").withDefaultValue("new HashMap<>()").withFinalModifier()
					.create();

		tableMap.entrySet().forEach(entry -> addPrivateField(entry.getValue(), entry.getKey()).create());

		final var serviceSet = new HashSet<String>();
		final BoundaryBean boundary = project.getBoundaryByDomainObject(dto.getDomainObject());

		// Add the declarations for all services
		if (boundary != null) {
			serviceSet.add(boundary.getInterfaceName());

			new ServiceDeclarationGenerator(this, boundary).addField(true, false);
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

			new ServiceDeclarationGenerator(this, listBoundary, boundary).addField(true, false);
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
			identifier = form.getName() + "(Window owner, " + pkDTOAttribute.getDomainAttribute().getJavaType().getName() + " id)";
		else if (formType == FormTypeEnumeration.ADD) {
			for (final FormField field : form.getAllFormFields()) {
				if (field.getFieldType() != FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
					continue;

				final DTOBean dto = field.getDTOAttribute().getReferencedDTOBean();
				final DTOBeanAttribute pkAttr = dto.getPKAttribute();
				final String typeName = pkAttr.getDomainAttribute().getJavaType().getName();

				String attributeName = field.getName().substring(COMBO_PREFIX.length());
				attributeName = attributeName.substring(0, 1).toLowerCase() + attributeName.substring(1);

				identifier = form.getName() + "(Window owner, " + typeName + " " + attributeName + ")";
				break;
			}
		}
		else
			identifier = form.getName() + "(Window owner)";

		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param owner\n");

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			b.append(" * @param id the primary key of the form object\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + identifier + "\n");
			b.append("{\n");
			b.append("super(owner, " + i18n.getI18N(form) + ");\n\n");
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
				b.append("super(owner, " + i18n.getI18N(form) + ");\n\n");
				b.append("this." + attributeName + " = " + attributeName + ";\n");
				break;
			}
		}
		else {
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + identifier + "\n");
			b.append("{\n");
			b.append("super(owner, " + i18n.getI18N(form) + ");\n");
		}

		b.append("\n");
		b.append(addInitializationOfAdditionalFields());
		b.append("setSize(" + form.getWidth() + ", " + form.getHeight() + ");\n");

		if (form.isTitleArea()) {
			b.append("\n");
			b.append("setTitleMessage(" + i18n.getI18N(form) + ");\n");

			if (formType == FormTypeEnumeration.READONLY)
				b.append("setTitleImage(ImageLoader.getImage(ImageLoader.IMG_VIEW_DATA_TITLE));\n");
			else if (formType == FormTypeEnumeration.ADD)
				b.append("setTitleImage(ImageLoader.getImage(ImageLoader.IMG_ADD_DATA_TITLE));\n");
			else if (formType == FormTypeEnumeration.UPDATE)
				b.append("setTitleImage(ImageLoader.getImage(ImageLoader.IMG_EDIT_DATA_TITLE));\n");
			else if (formType == FormTypeEnumeration.CREATE)
				b.append("setTitleImage(ImageLoader.getImage(ImageLoader.IMG_NEW_DATA_TITLE));\n");
		}

		b.append("\n");

		if (form.isResizable())
			b.append("setResizable(true);\n");
		else
			b.append("initStyle(StageStyle.UTILITY);\n");

		if (form.isModal())
			b.append("initModality(Modality.APPLICATION_MODAL);\n");
		else
			b.append("initModality(Modality.NONE);\n");

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
					.append(JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getDefaultInitializationFragment()));
		else
			form.getAllFormFields().forEach(field -> fieldInitialization
					.append(JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getCreateInitializationFragment()));

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractBaseDialog#createDialogArea()\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public Node createDialogArea()\n");
		b.append("{\n");
		b.append("final var panRoot = new VBox(10);\n");

		if (form.isTitleArea())
			b.append("final long start = System.currentTimeMillis();\n");

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

		// Sort the panels of the first row
		ECollections.sort(panelsOfFirstRow, new FormPanelComparator());

		// Check if the form needs a tab folder
		if (panelsOfFirstRow.size() > 1) {
			b.append("final var tabFolder1 = new TabPane();\n\n");
			b.append("panRoot.getChildren().add(tabFolder1);\n");

			if (!panelsOfSecondRow.isEmpty())
				b.append("VBox.setVgrow(tabFolder1, Priority.NEVER);\n\n");
			else
				b.append("VBox.setVgrow(tabFolder1, Priority.ALWAYS);\n\n");

			if (!lazyLoadMap1.isEmpty()) {
				b.append("tabFolder1.setOnMouseClicked(_ ->\n");
				b.append("{\n");
				b.append("if(tableLoadMap1.get(tabFolder1.getSelectionModel().getSelectedIndex()) == null)\n");
				b.append("return;\n\n");
				b.append("if(!tableLoadMap1.get(tabFolder1.getSelectionModel().getSelectedIndex()))\n");
				b.append("{\n");

				boolean firstItem = true;

				for (final Map.Entry<Integer, String> entry : lazyLoadMap1.entrySet()) {
					if (firstItem)
						firstItem = false;
					else
						b.append("else ");

					b.append("if(tabFolder1.getSelectionModel().getSelectedIndex() == " + (entry.getKey() - 1) + ")\n");
					b.append("{\n");
					b.append("tableLoadMap1.put(" + (entry.getKey() - 1) + ", true);\n");
					b.append(entry.getValue() + ".refreshView();\n");
					b.append("}\n");
				}

				b.append("}\n");
				b.append("});\n\n");
			}
		}

		// Add the panels of the first row
		for (final FormPanel panel : panelsOfFirstRow)
			b.append(addPanelToForm(panel, panelsOfFirstRow.size(), panelsOfSecondRow.size()));

		// Sort the panels of the second row
		ECollections.sort(panelsOfSecondRow, new FormPanelComparator());

		if (panelsOfSecondRow.size() > 1) {
			b.append("final var tabFolder2 = new TabPane();\n\n");
			b.append("panRoot.getChildren().add(tabFolder2);\n");
			b.append("VBox.setVgrow(tabFolder2, Priority.ALWAYS);\n\n");

			if (!lazyLoadMap2.isEmpty()) {
				b.append("tabFolder2.setOnMouseClicked(_ ->\n");
				b.append("{\n");
				b.append("if(tableLoadMap2.get(tabFolder2.getSelectionModel().getSelectedIndex()) == null)\n");
				b.append("return;\n\n");
				b.append("if(!tableLoadMap2.get(tabFolder2.getSelectionModel().getSelectedIndex()))\n");
				b.append("{\n");

				boolean firstItem = true;

				for (final Map.Entry<Integer, String> entry : lazyLoadMap2.entrySet()) {
					if (firstItem)
						firstItem = false;
					else
						b.append("else ");

					b.append("if(tabFolder2.getSelectionModel().getSelectedIndex() == " + (entry.getKey() - 1) + ")\n");
					b.append("{\n");
					b.append("tableLoadMap2.put(" + (entry.getKey() - 1) + ", true);\n");
					b.append(entry.getValue() + ".refreshView();\n");
					b.append("}\n");
				}

				b.append("}\n");
				b.append("});\n\n");
			}
		}

		// Add the panels of the second row
		for (final FormPanel panel : panelsOfSecondRow)
			b.append(addPanelToForm(panel, panelsOfFirstRow.size(), panelsOfSecondRow.size()));

		if (form.isTitleArea()) {
			final var msg = "Dialog initialized successfully in {0} seconds";

			b.append("\n");
			b.append("final long end = System.currentTimeMillis();\n");
			b.append("setInfoMessage(");
			b.append(i18n.getI18NMessage("msg_status_dialog_init", msg, "String.format(\"%.2f\", (double)(end - start) / 1000)"));
			b.append(");\n\n");
		}

		addDebugLog(b, "Dialog initialization finished");

		b.append("\n");
		b.append("return panRoot;\n");
		b.append("}\n\n");

		addMethod("Node createDialogArea()", b.toString());

		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dto.getDomainObject());
		final var declarationGenerator = new ServiceDeclarationGenerator(this, boundaryBean);

		if (declarationGenerator.needsCloseStatement()) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractBaseDialog#onClose()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void onClose()\n");
			b.append("{\n");

			new ServiceDeclarationGenerator(this, boundaryBean, b).addCloseStatement();

			b.append("}\n\n");

			addMethod("void onClose()", b.toString());
		}

		if (!fieldInitialization.isEmpty() || formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			addMethod("void onOpen()", createOnOpenMethod(fieldInitialization.toString()));

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractBaseDialog#onOKPressed()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void onOKPressed()\n");
		b.append("{\n");

		if (formType != FormTypeEnumeration.READONLY) {
			b.append("if(!validateUserInput())\n");
			b.append("return;\n\n");
			b.append("saveData();\n");
		}
		else
			b.append("close();\n");

		b.append("}\n\n");

		addMethod("void onOKPressed()", b.toString());

		if (formType != FormTypeEnumeration.READONLY) {
			addMethod("boolean validateUserInput()", createValidationMethod());

			addMethod("void saveData()", createFormDataSaveMethod());
		}

		addButtons();

		i18n.save();
	}

	/**
	 * Create the method to load all data and to initialize all form fields
	 * @param fieldInitializationFragement
	 * @return the generated content
	 */
	private String createOnOpenMethod(String fieldInitializationFragement) {
		final var b = new StringBuilder();
		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dto.getDomainObject());

		if (boundaryBean == null)
			throw new IllegalStateException(
					"The boundary bean for domain object '" + dto.getDomainObject().getName() + "' could not be found!");

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractBaseDialog#onOpen()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void onOpen()\n");
		b.append("{\n");
		b.append("getScene().setCursor(Cursor.WAIT);\n\n");
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
				b.append(tableName + ".refreshView();\n");
		}

		b.append("\n");
		b.append("getScene().setCursor(Cursor.DEFAULT);\n");
		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			addErrorLog(b, "Error while initializing dialog for object with id '{}'!", "ex", "id");
		else
			addErrorLog(b, "Error while initializing dialog!", "ex");

		b.append("\n");
		b.append("getScene().setCursor(Cursor.DEFAULT);\n");
		b.append("DialogUtil.openErrorDialog(this, ");
		b.append(i18n.getI18NMessage("msg_err_init_dialog", "Error while initializing from! Message: ") + ", ex);\n\n");
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

		b.append("/**\n");
		b.append(" * Check user input\n");
		b.append(" * @return true if validation was finished successfully!\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("private boolean validateUserInput()\n");
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

			final var message = "File that refers to field \"{0}\" hasn't been selected yet!";

			val.append(")\n");
			val.append("{\n");
			val.append("final String " + FIELD_LABEL_VALIDATION + " = " + i18n.getI18N(dataAttribute) + ";\n");
			val.append("final String message = ");
			val.append(i18n.getI18NMessage("msg_err_missing_file_ref", message, FIELD_LABEL_VALIDATION) + ";\n");

			if (form.isTitleArea()) {
				val.append("\n");
				val.append("setErrorMessage(message);\n");
			}
			else {
				val.append("final String title = " + i18n.getI18NMessage("msg_title_validation_error", "Validation error") + ";\n\n");
				val.append("DialogUtil.openWarningDialog(this, title, message);\n");
			}

			val.append("return false;\n");
			val.append("}\n\n");
		}

		// Validate the input of all form fields
		form.getAllFormFields().forEach(field -> val
				.append(JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getValidationFragment(form.isTitleArea())));

		if (!val.isEmpty()) {
			for (final FormField field : form.getAllFormFields()) {
				final FormFieldTypeEnumeration fieldType = field.getFieldType();

				if (fieldType == FormFieldTypeEnumeration.MULTI_LINE_TEXT || fieldType == FormFieldTypeEnumeration.SIMPLE_TEXT
						|| fieldType == FormFieldTypeEnumeration.DATE_TIME) {
					final String validationFragment = JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n)
							.getValidationFragment(form.isTitleArea());

					if (!validationFragment.isEmpty()) {
						b.append("var inputToCheck = \"\";\n\n");
						break;
					}
				}
			}

			b.append(val);
		}

		b.append("return true;\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * Add buttons to the button bar
	 */
	private void addButtons() {
		final var b = new StringBuilder();

		// Check if the method createButtons() must be overwritten!
		final boolean addMethod = form.getActions().stream().anyMatch(a -> a.getType() == ActionType.DOWNLOAD
				|| a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD);

		if (!addMethod)
			return;

		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.javafx.dialog.AbstractBaseDialog#createButtons()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("protected void createButtons()\n");
		b.append("{\n");
		b.append("super.createButtons();\n\n");

		for (final FormAction a : form.getActions()) {
			final var buttonName = "cmd" + a.getName().substring(0, 1).toUpperCase() + a.getName().substring(1);

			if (a.getType() == ActionType.DOWNLOAD) {
				b.append("final Button " + buttonName + " = addButton(DialogButtonType.DOWNLOAD, ");
				b.append(i18n.getI18NMessage("action_name_download", "Download") + ", false, false);\n\n");
				b.append(buttonName + ".setOnAction(_ ->\n{\n");
				b.append(new JavaFXFileHandlingGenerator(this, a, i18n).createDownloadMethodBody(true, "this"));
				b.append("});\n\n");
			}
			else if (a.getType() == ActionType.INDIRECT_UPLOAD) {
				final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();
				final String maxSize = uploadAttr.getMaxFileSize();
				final var message = "Selected file exceeds size limit of {0}!";
				final var convertedFileSize = "ByteConverter.convert(" + maxSize + ")";

				b.append("final Button " + buttonName + " = addButton(DialogButtonType.UPLOAD, ");
				b.append(i18n.getI18NMessage("action_name_upload", "Upload") + ", false, false);\n\n");
				b.append(buttonName + ".setOnAction(_ ->\n{\n");
				b.append("final var fc = new FileChooser();\n");
				b.append("fc.setTitle(" + i18n.getI18NMessage("file_upload_dialog", "Select file to upload") + ");\n\n");
				b.append("final File sourceFile = fc.showOpenDialog(this);\n\n");
				b.append("if(sourceFile == null)\n");
				b.append("return;\n\n");
				b.append("// Validate file length\n");
				b.append("if(sourceFile.length() > " + maxSize + ")\n");
				b.append("{\n");
				b.append("final String message = " + i18n.getI18NMessage("msg_err_upload_size", message, convertedFileSize) + ";\n");

				if (form.isTitleArea()) {
					b.append("\n");
					b.append("setErrorMessage(message);\n");
				}
				else {
					b.append("final String title = " + i18n.getI18NMessage("msg_title_validation_error", "Validation error") + ";\n\n");
					b.append("DialogUtil.openWarningDialog(this, title, message);\n");
				}

				b.append("return;\n");
				b.append("}\n\n");

				if (!project.isJavaSEApplication()) {
					b.append("// Upload file to remote server\n");
					b.append("final var uploadDialog = new FileUploadDialog(this, sourceFile);\n");
					b.append("uploadDialog.open();\n\n");
					b.append("if(uploadDialog.getException() != null)\n");
					b.append("{\n");
					b.append("DialogUtil.openErrorDialog(this, ");
					b.append(i18n.getI18NMessage("msg_err_upload", "Could not upload selected file!"));
					b.append(", uploadDialog.getException());\n");
					b.append("return;\n");
					b.append("}\n\n");
					b.append("final String serverPath = uploadDialog.getPath();\n\n");
				}

				addDebugLog(b, "Upload file");

				b.append("\n");
				b.append("try\n");
				b.append("{\n");

				if (!project.isJavaSEApplication()) {
					new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation("id", "serverPath");

					b.append("\n");
				}
				else
					new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation("id", "sourceFile.getAbsolutePath()");

				// Set fields that belong to documents
				b.append(addDocumentFieldLogic(a.getBoundaryMethod().getDomainAttribute().getDomainObject()));

				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");

				addErrorLog(b, "Error while performing file upload operation!", "ex");

				b.append("\n");
				b.append("DialogUtil.openErrorDialog(this, ");
				b.append(i18n.getI18NMessage("msg_err_upload", "Could not upload selected file!") + ", ex);\n");
				b.append("}\n");
				b.append("});\n\n");
			}
			else if (a.getType() == ActionType.DIRECT_UPLOAD) {
				final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();
				final String maxSize = uploadAttr.getMaxFileSize();
				final var message = "Selected file exceeds size limit of {0}!";
				final var convertedFileSize = "ByteConverter.convert(" + maxSize + ")";

				b.append("final Button " + buttonName + " = addButton(DialogButtonType.UPLOAD, ");
				b.append(i18n.getI18NMessage("action_name_browse", "Browse") + ", false, false);\n\n");
				b.append(buttonName + ".setOnAction(_ ->\n{\n");
				b.append("final var fc = new FileChooser();\n");
				b.append("fc.setTitle(" + i18n.getI18NMessage("file_upload_dialog", "Select file to upload") + ");\n\n");
				b.append("final File sourceFile = fc.showOpenDialog(this);\n\n");
				b.append("if(sourceFile == null)\n");
				b.append("return;\n\n");
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

				b.append("// Validate file length\n");
				b.append("if(sourceFile.length() > " + maxSize + ")\n");
				b.append("{\n");
				b.append("final String message = " + i18n.getI18NMessage("msg_err_upload_size", message, convertedFileSize) + ";\n");

				if (form.isTitleArea()) {
					b.append("\n");
					b.append("setErrorMessage(message);\n");
				}
				else {
					b.append("final String title = " + i18n.getI18NMessage("msg_title_validation_error", "Validation error") + ";\n\n");
					b.append("DialogUtil.openWarningDialog(this, title, message);\n");
				}

				b.append("return;\n");
				b.append("}\n\n");

				addDebugLog(b, "Upload file");

				b.append("\n");
				b.append(modelObjectName + "." + setter + "(");

				if (project.isBoundaryMode() || !uploadAttr.isLob())
					b.append("sourceFile.getAbsolutePath()");
				else if (uploadAttr.getJavaType().isType(JavaType.BYTE_ARRAY))
					b.append("FileUtil.getBytesFromFile(sourceFile)");
				else
					b.append("FileUtil.convertToByteArray(FileUtil.getBytesFromFile(sourceFile))");

				b.append(");\n");

				// Set fields that belong to documents
				b.append(addDocumentFieldLogic(a.getBoundaryMethod().getDomainAttribute().getDomainObject()));

				b.append("}\n");
				b.append("catch (final Exception e)\n");
				b.append("{\n");

				addErrorLog(b, "Error while performing file upload operation!", "e");

				b.append("\n");
				b.append("DialogUtil.openErrorDialog(this, ");
				b.append(i18n.getI18NMessage("msg_err_upload", "Could not upload selected file!") + ", e);\n");
				b.append("}\n");
				b.append("});\n\n");
			}

			b.append("\n");
		}

		b.append("}\n\n");

		addMethod("void createButtons()", b.toString());
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

				b.append("// Upload file to remote server\n");
				b.append("final var uploadDialog = new FileUploadDialog(this, new File(");
				b.append(modelObjectName + "." + dtoAttr.getModelGetterName() + "));\n");
				b.append("uploadDialog.open();\n\n");
				b.append("if(uploadDialog.getException() != null)\n");
				b.append("{\n");
				b.append("DialogUtil.openErrorDialog(null, ");
				b.append(i18n.getI18NMessage("msg_err_upload", "Could not upload selected file!") + ", uploadDialog.getException());\n");
				b.append("return;\n");
				b.append("}\n\n");
				b.append(modelObjectName + "." + dtoAttr.getModelSetterName() + "(uploadDialog.getPath());\n");

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
		b.append("/**\n");
		b.append(" * Save form data\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void saveData()\n");
		b.append("{\n");

		addDebugLog(b, "Perform save operation");

		b.append("\n");

		// Add fragments for saving data for all form fields
		for (final FormField field : form.getAllFormFields()) {
			final DTOBean fieldDTO = field.getDTOAttribute().getDTOBean();
			String objectName = modelObjectName;

			if (!fieldDTO.equals(dto))
				objectName = INIT_MODEL_OBJ_NAME_PREFIX + fieldDTO.getDomainObject().getName();

			b.append(JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getSaveDataFragment(objectName));
		}

		b.append("\n");
		b.append("getScene().setCursor(Cursor.WAIT);\n\n");
		b.append("try\n");
		b.append("{\n");

		if (!project.isJavaSEApplication() && (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE))
			b.append(createUploadFragment());

		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.CREATE || a.getType() == ActionType.UPDATE) {
				final String resultObjName = dto.getDomainObject().getLowerCaseName();

				b.append("// Save data\n");

				if (!a.getBoundaryMethod().getReturnType().isVoid())
					b.append(resultObjName + " = ");

				new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation(getSaveInvocationParameters());

				b.append("\n");
				b.append("getScene().setCursor(Cursor.DEFAULT);\n");
				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");

				addErrorLog(b, "Error while performing save operation!", "ex");

				b.append("\n");
				b.append("getScene().setCursor(Cursor.DEFAULT);\n");
				b.append("DialogUtil.openErrorDialog(this, ");
				b.append(i18n.getI18NMessage("msg_err_save", "Error while performing save operation!") + ", ex);\n");
				b.append("return;\n");
				b.append("}\n\n");
				b.append("returnCode = DialogButtonType.OK;\n");

				if ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && form.isOpenEditAfterCreate()) {
					for (final Form f : project.getAllFormsOfProject())
						if (f.getFormType() == FormTypeEnumeration.UPDATE && form.getDomainObject().equals(f.getDomainObject())) {
							final String getter = f.getDTO().getPKAttribute().getModelGetterName();
							b.append("\nnew " + f.getName() + "(" + form.getName() + ".this, " + resultObjName + "." + getter + ").open();\n");
						}
				}

				b.append("close();\n");
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
						b.append(f.getName() + ".setText(sourceFile.getName());\n");

					b.append(dto.getDomainObject().getLowerCaseName() + ".");
					b.append(f.getDTOAttribute().getModelSetterName() + "(sourceFile.getName());\n");
				}

				if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_SIZE) {
					if (f.isVisible())
						b.append(f.getName() + ".setText(ByteConverter.convert(sourceFile.length()));\n");

					b.append(dto.getDomainObject().getLowerCaseName() + "." + f.getDTOAttribute().getModelSetterName());

					if (attr.getJavaType().isInteger())
						b.append("((int) sourceFile.length());\n");
					else
						b.append("(sourceFile.length());\n");
				}
			}

		return b.toString();
	}

	/**
	 * @param panel
	 * @param addToTab
	 * @return the generated content
	 */
	private String createGridPane(FormPanel panel, boolean addToTab) {
		final var b = new StringBuilder();
		final boolean usesSecondCol = panel.getFields().stream()
				.anyMatch(f -> f.isVisible() && (f.getColIndex() != 1 || f.isSpanCols()));

		b.append("final var " + panel.getName() + " = new GridPane();\n");
		b.append(panel.getName() + ".setHgap(5.0);\n");
		b.append(panel.getName() + ".setVgap(5.0);\n");
		b.append(panel.getName() + ".setMaxHeight(Double.MAX_VALUE);\n");

		if (addToTab)
			b.append(panel.getName() + ".setPadding(new Insets(5));\n");

		b.append(panel.getName() + ".getColumnConstraints().add(new ColumnConstraints(USE_COMPUTED_SIZE, ");
		b.append("USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.NEVER, HPos.LEFT, false));\n");
		b.append(panel.getName() + ".getColumnConstraints().add(new ColumnConstraints(USE_COMPUTED_SIZE, ");
		b.append("USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, true));\n");

		if (usesSecondCol) {
			b.append(panel.getName() + ".getColumnConstraints().add(new ColumnConstraints(USE_COMPUTED_SIZE, ");
			b.append("USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.NEVER, HPos.LEFT, false));\n");
			b.append(panel.getName() + ".getColumnConstraints().add(new ColumnConstraints(USE_COMPUTED_SIZE, ");
			b.append("USE_COMPUTED_SIZE, USE_COMPUTED_SIZE, Priority.SOMETIMES, HPos.LEFT, true));\n");
		}

		b.append("\n");

		return b.toString();
	}

	/**
	 * Add the panel to the form
	 * @param panel
	 * @param columnsOfFirstRow
	 * @param columnsOfSecondRow
	 * @return the generated content
	 */
	private String addPanelToForm(FormPanel panel, int columnsOfFirstRow, int columnsOfSecondRow) {
		final var b = new StringBuilder();
		final boolean hasOneColumn = true;
		boolean addDataGrid = false;
		boolean addGridPane = false;
		boolean containsFields = true;

		if (panel.getBasePanel() == null) {
			int listFieldCount = 0;
			int standardFieldCount = 0;

			// Check if the panel contains list fields
			for (final FormField field : panel.getFields()) {
				if (!field.isVisible())
					continue;

				if (field.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST
						|| field.getFieldType() == FormFieldTypeEnumeration.LIST
						|| field.getFieldType() == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR)
					listFieldCount++;
				else
					standardFieldCount++;
			}

			if (listFieldCount > 1 || standardFieldCount > 0)
				addGridPane = true;

			if (listFieldCount == 0 && standardFieldCount == 0)
				containsFields = false;
		}
		else
			addDataGrid = true;

		// If there is no data grid and no form fields we won't add the panel!
		if (!addDataGrid && !containsFields)
			return "";

		boolean addToTab = false;

		if (panel.getRowIndex() == 1 && columnsOfFirstRow > 1)
			addToTab = true;

		if (panel.getRowIndex() == 2 && columnsOfSecondRow > 1)
			addToTab = true;

		if (!addDataGrid) {
			if (addGridPane)
				b.append(createGridPane(panel, addToTab));
			else {
				// Add a container for a single list field
				b.append("final var " + panel.getName() + " = new VBox();\n");
				b.append(panel.getName() + ".setMaxHeight(Double.MAX_VALUE);\n");
				b.append(panel.getName() + ".setPadding(new Insets(5));\n\n");
			}

			if (addToTab) {
				final var tabName = "tab" + panel.getRowIndex() + panel.getColIndex();

				b.append("final var " + tabName + " = new Tab();\n");
				b.append(tabName + ".setContent(" + panel.getName() + ");\n");
				b.append(tabName + ".setClosable(false);\n");
				b.append(tabName + ".setText(" + i18n.getI18N(panel) + ");\n\n");
				b.append("tabFolder" + panel.getRowIndex() + ".getTabs().add(" + tabName + ");\n");
			}
			else
				b.append("panRoot.getChildren().add(" + panel.getName() + ");\n");

			if ((addToTab && !addGridPane) || !addToTab) {
				if (panel.getRowIndex() != 1 || !addGridPane)
					b.append("VBox.setVgrow(" + panel.getName() + ", Priority.ALWAYS);\n");
				else
					b.append("VBox.setVgrow(" + panel.getName() + ", Priority.NEVER);\n");
			}
		}
		else {
			final String tableName = TABLE_PREFIX + Integer.toString(panel.getRowIndex()) + Integer.toString(panel.getColIndex());
			final AbstractDomainAssociation association = panel.getBasePanel().getAssociation();

			if (association instanceof ManyToManyAssociation
					|| association instanceof final OneToManyAssociation otm && !otm.isBidirectional())
				b.append(tableName + " = new " + panel.getBasePanel().getName() + "(this, id);\n");
			else if (panel.getForm().getFormType() == FormTypeEnumeration.READONLY)
				b.append(tableName + " = new " + panel.getBasePanel().getName() + "(this, id, true);\n");
			else
				b.append(tableName + " = new " + panel.getBasePanel().getName() + "(this, id, false);\n");

			b.append("\n");

			if (addToTab) {
				final var tabName = "tab" + panel.getRowIndex() + panel.getColIndex();

				b.append("final var " + tabName + " = new Tab();\n");
				b.append(tabName + ".setContent(" + tableName + ");\n");
				b.append(tabName + ".setClosable(false);\n");
				b.append(tabName + ".setText(" + i18n.getI18N(panel) + ");\n\n");
				b.append("tabFolder" + panel.getRowIndex() + ".getTabs().add(" + tabName + ");\n");
			}
			else {
				b.append("final var " + panel.getName() + " = new TitledPane();\n");
				b.append(panel.getName() + ".setContent(" + tableName + ");\n");
				b.append(panel.getName() + ".setMaxHeight(Double.MAX_VALUE);\n");
				b.append(panel.getName() + ".setText(" + i18n.getI18N(panel) + ");\n\n");
				b.append("panRoot.getChildren().add(" + panel.getName() + ");\n");
				b.append("VBox.setVgrow(" + panel.getName() + ", Priority.ALWAYS);\n");
			}
		}

		b.append("\n");

		if (!addDataGrid) {
			// Sort all form fields of this panel
			ECollections.sort(panel.getFields(), new FormFieldComparator());

			// Add the form fields to the panel
			panel.getFields().forEach(field -> b
					.append(JavaFXFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getFieldDefinitionFragment(hasOneColumn)));
		}

		return b.toString();
	}

}
