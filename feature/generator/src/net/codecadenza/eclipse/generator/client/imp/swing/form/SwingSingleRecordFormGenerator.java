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
package net.codecadenza.eclipse.generator.client.imp.swing.form;

import static net.codecadenza.eclipse.shared.Constants.APP_I18N_PROVIDER_CLASS;
import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.FIELD_LABEL_VALIDATION;
import static net.codecadenza.eclipse.shared.Constants.PACK_CLIENT_PANEL;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME;
import static net.codecadenza.eclipse.shared.Constants.PACK_JAVA_TIME_FORMAT;
import static net.codecadenza.eclipse.shared.Constants.TABLE_PREFIX;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import net.codecadenza.eclipse.generator.client.common.field.IFieldGenerator;
import net.codecadenza.eclipse.generator.client.common.form.AbstractSingleRecordFormGenerator;
import net.codecadenza.eclipse.generator.client.common.i18n.RichClientI18NGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceDeclarationGenerator;
import net.codecadenza.eclipse.generator.client.common.service.ServiceInvocationGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.file.SwingFileHandlingGenerator;
import net.codecadenza.eclipse.generator.client.imp.swing.form.field.SwingFieldGeneratorFactory;
import net.codecadenza.eclipse.generator.client.imp.swing.security.SwingSecurityHelper;
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
 * Generator for single-record forms of a Swing application
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class SwingSingleRecordFormGenerator extends AbstractSingleRecordFormGenerator {
	private final RichClientI18NGenerator i18n;
	private final EList<FormPanel> panelsOfFirstRow = new BasicEList<>();
	private final EList<FormPanel> panelsOfSecondRow = new BasicEList<>();
	private final HashMap<Integer, String> lazyLoadMap1 = new HashMap<>();
	private final HashMap<Integer, String> lazyLoadMap2 = new HashMap<>();
	private final HashMap<String, String> tableMap = new HashMap<>();
	private final HashMap<String, String> eagerLoadMap = new HashMap<>();
	private boolean hasLazyLoadPanels;
	private boolean hasLazyLoad1;
	private boolean hasLazyLoad2;
	private boolean addToolBar;

	/**
	 * Constructor
	 * @param form
	 */
	public SwingSingleRecordFormGenerator(Form form) {
		super(form);

		this.i18n = new RichClientI18NGenerator(project);

		// Check if a toolbar should be added!
		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.DOWNLOAD || a.getType() == ActionType.INDIRECT_UPLOAD
					|| a.getType() == ActionType.DIRECT_UPLOAD) {
				this.addToolBar = true;
				break;
			}

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
		importPackage("java.awt");
		importPackage("javax.swing");
		importPackage("javax.swing.border");

		if (project.isBoundaryMode()) {
			// Add import of the data transfer object that is mapped to this form
			importPackage(dto.getNamespace().toString());
		}
		else {
			importPackage(dto.getDomainObject().getNamespace().toString());
			importPackage("java.awt.event");
		}

		form.getActions().forEach(action -> {
			if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) {
				// Add imports for further data transfer objects that are used when creating initial one-to-many association objects!
				for (final MethodParameter param : action.getBoundaryMethod().getMethodParameters())
					if (param.getType() instanceof final DTOBean paramDTO) {
						String initDTOPackage;

						if (project.isBoundaryMode())
							initDTOPackage = paramDTO.getNamespace().toString();
						else
							initDTOPackage = paramDTO.getDomainObject().getNamespace().toString();

						importPackage(initDTOPackage);
					}

				// If we work directly with entities we must import packages of domain objects referenced by one-to-one associations!
				if (!project.isBoundaryMode())
					for (final DTOBeanAttribute attr : dto.getAttributes())
						if (attr.getAssociation() instanceof OneToOneAssociation)
							importPackage(attr.getAssociation().getTarget().getNamespace().toString());
			}
		});

		importPackage("net.codecadenza.runtime.richclient.swing.dialog");

		if (hasLazyLoadPanels)
			importPackage("java.util");

		// Search for additional imports concerning actions
		for (final FormAction a : form.getActions())
			if (a.getType() == ActionType.DIRECT_UPLOAD || a.getType() == ActionType.INDIRECT_UPLOAD
					|| a.getType() == ActionType.DOWNLOAD) {
				importPackage("net.codecadenza.runtime.richclient.swing.image");

				if (a.getType() == ActionType.DIRECT_UPLOAD || a.getType() == ActionType.INDIRECT_UPLOAD) {
					importPackage("java.io");
					importPackage("net.codecadenza.runtime.file");
				}

				if (project.isJavaSEApplication()) {
					if (a.getType() == ActionType.DOWNLOAD)
						importPackage("java.io");

					if (a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DOWNLOAD)
						importPackage("net.codecadenza.runtime.file");

					if (a.getType() == ActionType.DIRECT_UPLOAD && !project.isBoundaryMode()
							&& a.getBoundaryMethod().getDomainAttribute().isLob())
						importPackage("net.codecadenza.runtime.file");
				}
				else
					importPackage("net.codecadenza.runtime.richclient.swing.file");
			}

		// Analyze the form to add all necessary imports
		for (final FormPanel panel : form.getFormPanels()) {
			if (panel.getBasePanel() != null) {
				importPackage(project.getClientNamespace().toString() + PACK_CLIENT_PANEL);

				if (panel.getColIndex() != 1)
					importPackage("java.awt.event");

				continue;
			}

			// Add imports for all form fields
			panel.getFields().forEach(field -> SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addImports());
		}

		// Check if further imports for formatting data are necessary
		form.getAllFormFields().forEach(field -> {
			final IFieldGenerator fieldGenerator = SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n);

			if (fieldGenerator.needsDateFormatter() || fieldGenerator.needsDateTimeFormatter()
					|| fieldGenerator.needsDecimalFormatter()) {
				if (fieldGenerator.needsDecimalFormatter())
					importPackage("java.text");
				else
					importPackage(PACK_JAVA_TIME_FORMAT);

				importPackage("net.codecadenza.runtime.richclient.format");
			}

			if (field.isAddFormLinkToLabel()) {
				importClass("org.jdesktop.swingx.JXHyperlink");

				addImports(new SwingSecurityHelper(project).getSecurityImports());
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

		if (formType == FormTypeEnumeration.READONLY && !addToolBar)
			b.append(" extends JTitleAreaDialog");
		else
			b.append(" extends JStatusTitleAreaDialog");
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
				useDateFormatter = SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDateFormatter();

			if (!useDateTimeFormatter)
				useDateTimeFormatter = SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDateTimeFormatter();

			if (!useDecimalFormatter)
				useDecimalFormatter = SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).needsDecimalFormatter();

			// We can stop further investigation if all formatter objects are necessary!
			if (useDecimalFormatter && useDateFormatter && useDateTimeFormatter)
				break;
		}

		addPrivateConstant(JavaType.LONG, "serialVersionUID", "1L").create();

		if (useDecimalFormatter || useDateFormatter || useDateTimeFormatter) {
			addPrivateField("FormatDTO", "userFormat").withDefaultValue("FormatPreferencesManager.getFormatDTO()").withFinalModifier()
					.create();

			if (useDecimalFormatter)
				addPrivateField("DecimalFormat", "decimalFormat").withDefaultValue("new DecimalFormat(userFormat.getDecimalFormat())")
						.withFinalModifier().create();

			if (useDateFormatter) {
				final var formatter = "DateTimeFormatter.ofPattern(userFormat.getDateFormat()).withZone(" + PACK_JAVA_TIME
						+ ".ZoneId.systemDefault())";

				addPrivateField("DateTimeFormatter", "dateFormat").withDefaultValue(formatter).withTransientModifier().withFinalModifier()
						.create();
			}

			if (useDateTimeFormatter) {
				final var formatter = "DateTimeFormatter.ofPattern(userFormat.getDateTimeFormat()).withZone(" + PACK_JAVA_TIME
						+ ".ZoneId.systemDefault())";

				addPrivateField("DateTimeFormatter", "dateTimeFormat").withDefaultValue(formatter).withTransientModifier()
						.withFinalModifier().create();
			}
		}

		// Add form field declarations
		form.getAllFormFields()
				.forEach(field -> SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).addFieldDeclaration());

		// Add the ID attribute
		final DTOBeanAttribute pkDTOAttr = dto.getPKAttribute();

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			if (pkDTOAttr.getDomainAttribute().getJavaType().getNamespace() != null)
				importPackage(pkDTOAttr.getDomainAttribute().getJavaType().getNamespace().toString());

			addPrivateField(pkDTOAttr.getDomainAttribute().getJavaType().getName(), "id").withFinalModifier().create();
		}

		// Add the declaration for the model object
		addPrivateField(dto.getModelClassName(), modelObjectName).setTransientModifier(!project.isBoundaryMode())
				.withDefaultValue("new " + dto.getModelClassName() + "()").create();

		additionalDTOs.stream().forEach(
				addDTO -> addPrivateField(addDTO.getModelClassName(), INIT_MODEL_OBJ_NAME_PREFIX + addDTO.getDomainObject().getName())
						.setTransientModifier(!project.isBoundaryMode()).create());

		if (hasLazyLoad1)
			addPrivateField("HashMap<Integer, Boolean>", "tableLoadMap1").withDefaultValue("new HashMap<>()").withFinalModifier()
					.create();

		if (hasLazyLoad2)
			addPrivateField("HashMap<Integer, Boolean>", "tableLoadMap2").withDefaultValue("new HashMap<>()").withFinalModifier()
					.create();

		tableMap.entrySet().forEach(entry -> addPrivateField(entry.getValue(), entry.getKey()).create());

		if (addToolBar)
			addPrivateField("JToolBar", "toolBar").create();

		if (formType != FormTypeEnumeration.READONLY)
			addPrivateField("SwingWorker<Void, Void>", "saveWorker").withTransientModifier().create();

		final var serviceSet = new HashSet<String>();
		final BoundaryBean boundary = project.getBoundaryByDomainObject(dto.getDomainObject());

		// Add the declaration for all services
		if (boundary != null) {
			serviceSet.add(boundary.getInterfaceName());

			new ServiceDeclarationGenerator(this, boundary).addField(true, true);
		}

		for (final FormField field : form.getAllFormFields()) {
			if (formType == FormTypeEnumeration.READONLY || !field.isVisible() || field.isReadonly())
				continue;

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

			new ServiceDeclarationGenerator(this, listBoundary, boundary).addField(true, true);
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
			identifier = form.getName() + "(Component parentComponent, " + pkDTOAttribute.getDomainAttribute().getJavaType().getName()
					+ " id)";
		else if (formType == FormTypeEnumeration.ADD) {
			for (final FormField field : form.getAllFormFields()) {
				if (field.getFieldType() != FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM)
					continue;

				final DTOBean refDTO = field.getDTOAttribute().getReferencedDTOBean();
				final DTOBeanAttribute pkAttr = refDTO.getPKAttribute();
				final String typeName = pkAttr.getDomainAttribute().getJavaType().getName();

				String attributeName = field.getName().substring(COMBO_PREFIX.length());
				attributeName = attributeName.substring(0, 1).toLowerCase() + attributeName.substring(1);

				identifier = form.getName() + "(Component parentComponent, " + typeName + " " + attributeName + ")";
				break;
			}
		}
		else
			identifier = form.getName() + "(Component parentComponent)";

		// Add the constructor
		b.append("/**\n");
		b.append(" * Create the dialog\n");
		b.append(" * @param parentComponent\n");

		if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE) {
			b.append(" * @param id the primary key of the form object\n");
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + identifier + "\n");
			b.append("{\n");
			b.append("super(parentComponent, true);\n\n");
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
				b.append("super(parentComponent, true);\n\n");
				b.append("this." + attributeName + " = " + attributeName + ";\n");
				break;
			}
		}
		else {
			b.append(" */\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public " + identifier + "\n");
			b.append("{\n");
			b.append("super(parentComponent, true);\n");
		}

		b.append("\n");
		b.append(addInitializationOfAdditionalFields());

		if (form.isModal())
			b.append("setModal(true);\n");
		else
			b.append("setModal(false);\n");

		if (form.isResizable())
			b.append("setResizable(true);\n");

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
		final var msg = "Dialog initialized successfully in {0} seconds";

		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			form.getAllFormFields().forEach(field -> fieldInitialization
					.append(SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getDefaultInitializationFragment()));
		else
			form.getAllFormFields().forEach(field -> fieldInitialization
					.append(SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getCreateInitializationFragment()));

		var b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.");
		b.append("JTitleAreaDialog#createContents(javax.swing.JPanel)\n");
		b.append(" */\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("@Override\n");
		b.append("public void createContents(JPanel panContent)\n");
		b.append("{\n");
		b.append("final long start = System.currentTimeMillis();\n\n");

		addDebugLog(b, "Initialize dialog");

		b.append("\n");
		b.append("setSize(" + form.getWidth() + ", " + form.getHeight() + ");\n");
		b.append("setLocationRelativeTo(null);\n");
		b.append("setTitle(" + i18n.getI18N(form) + ");\n");
		b.append("setTitleMessage(" + i18n.getI18N(form) + ");\n\n");
		b.append("final var gblContentPane = new GridBagLayout();\n");
		b.append("gblContentPane.columnWidths = new int[]{0, 0};\n");
		b.append("gblContentPane.columnWeights = new double[]{1.0, Double.MIN_VALUE};\n");

		if (addToolBar) {
			if (!panelsOfSecondRow.isEmpty()) {
				b.append("gblContentPane.rowHeights = new int[]{0, 0, 0};\n");
				b.append("gblContentPane.rowWeights = new double[]{0, 0.1, 1.0};\n");
			}
			else {
				b.append("gblContentPane.rowHeights = new int[]{0, 0};\n");
				b.append("gblContentPane.rowWeights = new double[]{0, 1.0};\n");
			}
		}
		else if (!panelsOfSecondRow.isEmpty()) {
			b.append("gblContentPane.rowHeights = new int[]{0, 0};\n");
			b.append("gblContentPane.rowWeights = new double[]{0.1, 1.0};\n");
		}
		else {
			b.append("gblContentPane.rowHeights = new int[]{0};\n");
			b.append("gblContentPane.rowWeights = new double[]{1.0};\n");
		}

		b.append("\n");
		b.append("panContent.setLayout(gblContentPane);\n\n");

		if (addToolBar)
			b.append(addToolBarItems());

		if (!lazyLoadMap1.isEmpty() || !lazyLoadMap2.isEmpty()) {
			b.append("// Initialize lazy load map\n");

			for (final int colIndex : lazyLoadMap1.keySet())
				b.append("tableLoadMap1.put(" + (colIndex - 1) + ", false);\n");

			for (final int colIndex : lazyLoadMap2.keySet())
				b.append("tableLoadMap2.put(" + (colIndex - 1) + ", false);\n");

			b.append("\n");
		}

		// Sort all panels of the first row
		ECollections.sort(panelsOfFirstRow, new FormPanelComparator());

		// Check if the form needs a tab folder
		if (panelsOfFirstRow.size() > 1) {
			b.append("final var tabFolder1 = new JTabbedPane(JTabbedPane.TOP);\n\n");
			b.append("final var gbcTabFolder1 = new GridBagConstraints();\n");
			b.append("gbcTabFolder1.insets = new Insets(5, 0, 0, 0);\n");
			b.append("gbcTabFolder1.fill = GridBagConstraints.BOTH;\n");
			b.append("gbcTabFolder1.gridx = 0;\n");

			if (addToolBar)
				b.append("gbcTabFolder1.gridy = 1;\n");
			else
				b.append("gbcTabFolder1.gridy = 0;\n");

			b.append("\n");
			b.append("panContent.add(tabFolder1, gbcTabFolder1);\n\n");

			if (!lazyLoadMap1.isEmpty()) {
				b.append("tabFolder1.addMouseListener(new MouseAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void mousePressed(MouseEvent e)\n");
				b.append("{\n");
				b.append("if(tableLoadMap1.get(tabFolder1.getSelectedIndex()) == null)\n");
				b.append("return;\n\n");
				b.append("if(!tableLoadMap1.get(tabFolder1.getSelectedIndex()))\n");
				b.append("{\n");

				boolean firstItem = true;

				for (final Map.Entry<Integer, String> entry : lazyLoadMap1.entrySet()) {
					if (firstItem)
						firstItem = false;
					else
						b.append("else ");

					b.append("if(tabFolder1.getSelectedIndex() == " + (entry.getKey() - 1) + ")\n");
					b.append("{\n");
					b.append("tableLoadMap1.put(" + (entry.getKey() - 1) + ", true);\n");
					b.append(entry.getValue() + ".performFetch();\n");
					b.append("}\n");
				}

				b.append("}\n");
				b.append("}\n");
				b.append("});\n\n");
			}
		}

		// Add the panels of the first row
		for (final FormPanel panel : panelsOfFirstRow)
			b.append(addPanelToForm(panel, panelsOfFirstRow.size(), panelsOfFirstRow.size()));

		// Sort all panels of the second row
		ECollections.sort(panelsOfSecondRow, new FormPanelComparator());

		// Add a tab folder for the second row if the first row uses a tab folder
		if ((panelsOfFirstRow.size() > 1 && !panelsOfSecondRow.isEmpty()) || panelsOfSecondRow.size() > 1) {
			b.append("final var tabFolder2 = new JTabbedPane(JTabbedPane.TOP);\n\n");
			b.append("final var gbcTabFolder2 = new GridBagConstraints();\n");
			b.append("gbcTabFolder2.insets = new Insets(0, 0, 0, 0);\n");
			b.append("gbcTabFolder2.fill = GridBagConstraints.BOTH;\n");
			b.append("gbcTabFolder2.gridx = 0;\n");

			if (addToolBar)
				b.append("gbcTabFolder2.gridy = 2;\n");
			else
				b.append("gbcTabFolder2.gridy = 1;\n");

			b.append("\n");
			b.append("panContent.add(tabFolder2, gbcTabFolder2);\n\n");

			if (!lazyLoadMap2.isEmpty()) {
				b.append("tabFolder2.addMouseListener(new MouseAdapter()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see java.awt.event.MouseAdapter#mousePressed(java.awt.event.MouseEvent)\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("public void mousePressed(MouseEvent e)\n");
				b.append("{\n");
				b.append("if(tableLoadMap2.get(tabFolder2.getSelectedIndex()) == null)\n");
				b.append("return;\n\n");
				b.append("if(!tableLoadMap2.get(tabFolder2.getSelectedIndex()))\n");
				b.append("{\n");

				boolean firstItem = true;

				for (final Map.Entry<Integer, String> entry : lazyLoadMap2.entrySet()) {
					if (firstItem)
						firstItem = false;
					else
						b.append("else ");

					b.append("if(tabFolder2.getSelectedIndex() == " + (entry.getKey() - 1) + ")\n");
					b.append("{\n");
					b.append("tableLoadMap2.put(" + (entry.getKey() - 1) + ", true);\n");
					b.append(entry.getValue() + ".performFetch();\n");
					b.append("}\n");
				}

				b.append("}\n");
				b.append("}\n");
				b.append("});\n\n");
			}
		}

		// Add the panels of the second row
		for (final FormPanel panel : panelsOfSecondRow)
			b.append(addPanelToForm(panel, panelsOfSecondRow.size(), panelsOfFirstRow.size()));

		final boolean initDialog = !fieldInitialization.isEmpty() || formType == FormTypeEnumeration.UPDATE
				|| formType == FormTypeEnumeration.READONLY;

		if (initDialog) {
			b.append("// Initialize the dialog\n");
			b.append("initDialog();\n");
		}

		final BoundaryBean boundaryBean = project.getBoundaryByDomainObject(dto.getDomainObject());
		final var declarationGenerator = new ServiceDeclarationGenerator(this, boundaryBean, b);

		if (declarationGenerator.needsCloseStatement()) {
			b.append("\n");
			b.append("addWindowListener(new WindowAdapter()\n");
			b.append("{\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.awt.event.WindowAdapter#windowClosing(java.awt.event.WindowEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void windowClosing(WindowEvent e)\n");
			b.append("{\n");

			declarationGenerator.addCloseStatement();

			b.append("}\n\n");
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see java.awt.event.WindowAdapter#windowClosed(java.awt.event.WindowEvent)\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append("public void windowClosed(WindowEvent e)\n");
			b.append("{\n");

			declarationGenerator.addCloseStatement();

			b.append("}\n");
			b.append("});\n");
		}

		b.append("\n");
		b.append("final long end = System.currentTimeMillis();\n");
		b.append("setInformationMessage(");
		b.append(i18n.getI18NMessage("msg_status_dialog_init", msg, "String.format(\"%.2f\", (double)(end - start) / 1000)"));
		b.append(");\n\n");

		addDebugLog(b, "Dialog initialization finished");

		b.append("}\n\n");

		addMethod("void createContents(JPanel panContent)", b.toString());

		b = new StringBuilder();
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#onOKClicked()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append(getAnnotationForGeneratedElement());
		b.append("public void onOKClicked()\n");
		b.append("{\n");

		if (formType == FormTypeEnumeration.READONLY) {
			b.append("dispose();\n");
			b.append("setReturnCode(RETURN_CODE_OK);\n");
		}
		else {
			b.append("if(!validateUserInput())\n");
			b.append("return;\n\n");
			b.append("saveData();\n");
		}

		b.append("}\n\n");

		addMethod("void onOKClicked()", b.toString());

		if (formType != FormTypeEnumeration.READONLY) {
			b = new StringBuilder();
			b.append("/* (non-Javadoc)\n");
			b.append(" * @see net.codecadenza.runtime.richclient.swing.dialog.JTitleAreaDialog#onCancelClicked()\n");
			b.append(" */\n");
			b.append("@Override\n");
			b.append(getAnnotationForGeneratedElement());
			b.append("public void onCancelClicked()\n");
			b.append("{\n");
			b.append("if(saveWorker != null && saveWorker.getState() == SwingWorker.StateValue.STARTED)\n");
			b.append("saveWorker.cancel(true);\n\n");
			b.append("super.onCancelClicked();\n");
			b.append("}\n\n");

			addMethod("void onCancelClicked()", b.toString());
		}

		if (initDialog)
			addMethod("void initDialog()", createInitDialogMethod(fieldInitialization.toString()));

		if (formType != FormTypeEnumeration.READONLY) {
			addMethod("boolean validateUserInput()", createValidationMethod());

			addMethod("void saveData()", createFormDataSaveMethod());
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
				b.append(tableName + ".performFetch();\n");
		}

		b.append("}\n");
		b.append("catch (final Exception ex)\n");
		b.append("{\n");

		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			addErrorLog(b, "Error while initializing dialog for object with id '{}'!", "ex", "id");
		else
			addErrorLog(b, "Error while initializing dialog!", "ex");

		b.append("\n");
		b.append("JOptionPane.showMessageDialog(this, ");
		b.append(i18n.getI18NMessage("msg_err_init_dialog", "Error while initializing dialog! Message: "));
		b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_init_dialog", "Dialog initialization"));
		b.append(", JOptionPane.WARNING_MESSAGE);\n");
		b.append("dispose();\n");
		b.append("}\n");
		b.append("}\n\n");

		return b.toString();
	}

	/**
	 * Create the method to validate the user input
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

			final var msg = "File that refers to field \"{0}\" hasn't been selected yet!";

			val.append("// Check if user has already uploaded a file for respective field!\n");
			val.append("if(" + modelObjectName + "." + dataAttribute.getModelGetterName() + " == null");

			if (project.isBoundaryMode())
				val.append(" || " + modelObjectName + "." + dataAttribute.getModelGetterName() + ".isEmpty()");

			val.append(")\n");
			val.append("{\n");
			val.append("final String " + FIELD_LABEL_VALIDATION + " = " + i18n.getI18N(dataAttribute) + ";\n");
			val.append("setTitleMessage(" + i18n.getI18NMessage("msg_title_err_missing_file_ref", "Missing file reference") + ");\n");
			val.append("setErrorMessage(" + i18n.getI18NMessage("msg_err_missing_file_ref", msg, FIELD_LABEL_VALIDATION) + ");\n");
			val.append("return false;\n");
			val.append("}\n\n");
		}

		// Validate the input of all form fields
		form.getAllFormFields().forEach(
				field -> val.append(SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getValidationFragment(true)));

		if (!val.isEmpty()) {
			for (final FormField field : form.getAllFormFields()) {
				final FormFieldTypeEnumeration fieldType = field.getFieldType();

				if (fieldType == FormFieldTypeEnumeration.MULTI_LINE_TEXT || fieldType == FormFieldTypeEnumeration.SIMPLE_TEXT
						|| field.getFieldType() == FormFieldTypeEnumeration.DATE || fieldType == FormFieldTypeEnumeration.DATE_TIME) {
					final String validationFragment = SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n)
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
	 * Add the toolbar and its items
	 * @return the generated content
	 */
	private String addToolBarItems() {
		final var b = new StringBuilder();
		b.append("toolBar = new JToolBar();\n");
		b.append("toolBar.setFloatable(false);\n");
		b.append("toolBar.setRollover(true);\n\n");
		b.append("final var gbcToolBar = new GridBagConstraints();\n");
		b.append("gbcToolBar.insets = new Insets(0, 0, 0, 0);\n");
		b.append("gbcToolBar.fill = GridBagConstraints.HORIZONTAL;\n");
		b.append("gbcToolBar.gridx = 0;\n");
		b.append("gbcToolBar.gridy = 0;\n\n");
		b.append("panContent.add(toolBar, gbcToolBar);\n\n");

		for (final FormAction a : form.getActions()) {
			if (a.getType() == ActionType.DOWNLOAD) {
				final var toolItemName = a.getName() + "Item";

				b.append("final var " + toolItemName + " = new JButton();\n");
				b.append(toolItemName + ".setText(" + i18n.getI18NMessage("action_name_download", "Download") + ");\n");
				b.append(toolItemName + ".setIcon(ImageLoader.getImage(ImageLoader.DOWNLOAD));\n\n");
				b.append(toolItemName + ".addActionListener(_ ->\n");
				b.append("{\n");
				b.append(new SwingFileHandlingGenerator(this, a, i18n).createDownloadMethodBody(true, form.getName()));
				b.append("});\n\n");
				b.append("toolBar.add(" + toolItemName + ");\n");

			}
			else if (a.getType() == ActionType.INDIRECT_UPLOAD) {
				final var toolItemName = a.getName() + "Item";
				final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();
				final String maxSize = uploadAttr.getMaxFileSize();
				final var msgMaxFileSize = "Selected file exceeds size limit of {0}!";
				final var convertedFileSize = "ByteConverter.convert(" + maxSize + ")";

				b.append("final var " + toolItemName + " = new JButton();\n");
				b.append(toolItemName + ".setText(" + i18n.getI18NMessage("action_name_upload", "Upload") + ");\n");
				b.append(toolItemName + ".setIcon(ImageLoader.getImage(ImageLoader.UPLOAD));\n\n");
				b.append(toolItemName + ".addActionListener(_ ->\n");
				b.append("{\n");
				b.append("final var fc = new JFileChooser();\n");
				b.append("fc.setFileSelectionMode(JFileChooser.FILES_ONLY);\n");
				b.append("fc.setDialogTitle(" + i18n.getI18NMessage("file_upload_dialog", "Select file to upload!") + ");\n\n");
				b.append("if(JFileChooser.APPROVE_OPTION != fc.showOpenDialog(null))\n");
				b.append("return;\n\n");
				b.append("// Validate file length\n");
				b.append("if(new File(fc.getSelectedFile().getAbsolutePath()).length() > " + maxSize + ")\n");
				b.append("{\n");
				b.append("setErrorMessage(");
				b.append(i18n.getI18NMessage("msg_err_upload_size", msgMaxFileSize, convertedFileSize) + ");\n");
				b.append("return;\n");
				b.append("}\n\n");

				addDebugLog(b, "Upload file");

				b.append("\n");
				b.append("setBusy(true);\n");
				b.append("setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));\n\n");
				b.append("final var w = new SwingWorker<Void, Void>()\n");
				b.append("{\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see javax.swing.SwingWorker#doInBackground()\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("protected Void doInBackground() throws Exception\n");
				b.append("{\n");
				b.append("final String path = fc.getSelectedFile().getAbsolutePath();\n\n");
				b.append("// Upload data to server\n");

				if (project.isBoundaryMode())
					new ServiceDeclarationGenerator(this, a.getBoundaryMethod(), b).addLocalVariable();

				if (!project.isJavaSEApplication()) {
					b.append("final var uploadOperation = new FileUploadOperation(" + form.getName() + ".this);\n\n");
					b.append("final String serverPath = uploadOperation.startUpload(path);\n");

					new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation("id", "serverPath");
				}
				else
					new ServiceInvocationGenerator(a.getBoundaryMethod(), b).addInvocation("id", "path");

				b.append("\n");

				// Set fields that belong to documents
				b.append(addDocumentFieldLogic(true, a.getBoundaryMethod().getDomainAttribute().getDomainObject()));

				b.append("return null;\n");
				b.append("}\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see javax.swing.SwingWorker#done()\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("protected void done()\n");
				b.append("{\n");
				b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
				b.append("setBusy(false);\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("// We have to call method get() in order to see any exception!\n");
				b.append("get();\n");
				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");
				b.append("if(ex instanceof InterruptedException)\n");
				b.append("Thread.currentThread().interrupt();\n\n");

				addErrorLog(b, "Error while performing file upload operation!", "ex");

				final var msg = "Could not upload selected file! Message: ";

				b.append("\n");
				b.append("JOptionPane.showMessageDialog(" + a.getForm().getName() + ".this, ");
				b.append(i18n.getI18NMessage("msg_err_upload", msg));
				b.append(" + ex.getMessage(), " + i18n.getI18NMessage("msg_title_upload", "Upload file"));
				b.append(", JOptionPane.WARNING_MESSAGE);\n");
				b.append("return;\n");
				b.append("}\n\n");
				b.append("setStatusInfoMessage(" + i18n.getI18NMessage("status_upload_finished", "Upload finished!") + ");\n");
				b.append("}\n");
				b.append("};\n\n");
				b.append("w.execute();\n");
				b.append("});\n\n");
				b.append("toolBar.add(" + toolItemName + ");\n");
			}
			else if (a.getType() == ActionType.DIRECT_UPLOAD) {
				final var toolItemName = a.getName() + "Item";
				final DomainAttribute uploadAttr = a.getBoundaryMethod().getDomainAttribute();
				final String maxSize = uploadAttr.getMaxFileSize();
				final var msgMaxFileSize = "Selected file exceeds size limit of {0}!";
				final var convertedFileSize = "ByteConverter.convert(" + maxSize + ")";

				b.append("final var " + toolItemName + " = new JButton();\n");
				b.append(toolItemName + ".setText(" + i18n.getI18NMessage("action_name_browse", "Browse") + ");\n");
				b.append(toolItemName + ".setIcon(ImageLoader.getImage(ImageLoader.UPLOAD));\n\n");
				b.append(toolItemName + ".addActionListener(_ ->\n");
				b.append("{\n");
				b.append("final var fc = new JFileChooser();\n");
				b.append("fc.setFileSelectionMode(JFileChooser.FILES_ONLY);\n");
				b.append("fc.setDialogTitle(" + i18n.getI18NMessage("file_upload_dialog", "Select file to upload!") + ");\n\n");
				b.append("if(JFileChooser.APPROVE_OPTION != fc.showOpenDialog(null))\n");
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

				b.append("final String path = fc.getSelectedFile().getAbsolutePath();\n\n");
				b.append("// Validate file length\n");
				b.append("if(new File(path).length() > " + maxSize + ")\n");
				b.append("{\n");
				b.append("setErrorMessage(");
				b.append(i18n.getI18NMessage("msg_err_upload_size", msgMaxFileSize, convertedFileSize) + ");\n");
				b.append("return;\n");
				b.append("}\n\n");

				addDebugLog(b, "Upload file");

				b.append("\n");
				b.append(modelObjectName + "." + setter + "(");

				if (project.isBoundaryMode() || !uploadAttr.isLob())
					b.append("path");
				else if (uploadAttr.getJavaType().isType(JavaType.BYTE_ARRAY))
					b.append("FileUtil.getBytesFromFile(new File(path))");
				else
					b.append("FileUtil.convertToByteArray(FileUtil.getBytesFromFile(new File(path)))");

				b.append(");\n");

				// Set fields that belong to documents
				b.append(addDocumentFieldLogic(false, a.getBoundaryMethod().getDomainAttribute().getDomainObject()));

				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");

				addErrorLog(b, "Error while saving file content to field!", "ex");

				b.append("\n");
				b.append("setErrorMessage(" + i18n.getI18NMessage("msg_err_upload", "Could not upload selected file! Message: "));
				b.append(" + ex.getMessage());\n");
				b.append("}\n");
				b.append("});\n\n");
				b.append("toolBar.add(" + toolItemName + ");\n");
			}

			b.append("\n");
		}

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
					b.append("if(" + modelObjectName + "." + dtoAttr.getModelGetterName() + "!= null && !");
					b.append(modelObjectName + "." + dtoAttr.getModelGetterName() + ".isEmpty())\n");
					b.append("{\n");
				}

				final var operationName = "op" + dtoAttr.getUpperCaseName();
				final var pathName = dtoAttr.getName() + "Path";

				b.append("// Upload data to server\n");
				b.append("final var " + operationName + " = new FileUploadOperation(" + form.getName() + ".this);\n\n");
				b.append("final String " + pathName + " = " + operationName + ".startUpload(");
				b.append(modelObjectName + "." + dtoAttr.getModelGetterName() + ");\n\n");
				b.append(modelObjectName + "." + dtoAttr.getModelSetterName() + "(" + pathName + ");\n");

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
		b.append("if(saveWorker != null && saveWorker.getState() == SwingWorker.StateValue.STARTED)\n");
		b.append("{\n");
		b.append("JOptionPane.showMessageDialog(this, ");
		b.append(i18n.getI18NMessage("msg_err_thread_started", "Operation to save data has been already started!"));
		b.append(", " + i18n.getI18NMessage("msg_title_save", "Save operation") + ", JOptionPane.INFORMATION_MESSAGE);\n");
		b.append("return;\n");
		b.append("}\n\n");

		addDebugLog(b, "Perform save operation");

		b.append("\n");

		// Add fragments for saving data for all form fields
		for (final FormField field : form.getAllFormFields()) {
			final DTOBean fieldDTO = field.getDTOAttribute().getDTOBean();
			String objectName = modelObjectName;

			if (!fieldDTO.equals(dto))
				objectName = INIT_MODEL_OBJ_NAME_PREFIX + fieldDTO.getDomainObject().getName();

			b.append(SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getSaveDataFragment(objectName));
		}

		b.append("\nsetBusy(true);\n");
		b.append("setCursor(Cursor.getPredefinedCursor(Cursor.WAIT_CURSOR));\n\n");

		if (addToolBar) {
			b.append("// Disable all toolbar actions!\n");
			b.append("for(int i = 0; i < toolBar.getComponentCount(); i++)\n");
			b.append("toolBar.getComponent(i).setEnabled(false);\n\n");
		}

		b.append("saveWorker = new SwingWorker<>()\n");
		b.append("{\n");
		b.append("/* (non-Javadoc)\n");
		b.append(" * @see javax.swing.SwingWorker#doInBackground()\n");
		b.append(" */\n");
		b.append("@Override\n");
		b.append("protected Void doInBackground() throws Exception\n");
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
				b.append("return null;\n");
				b.append("}\n");
				b.append("/* (non-Javadoc)\n");
				b.append(" * @see javax.swing.SwingWorker#done()\n");
				b.append(" */\n");
				b.append("@Override\n");
				b.append("protected void done()\n");
				b.append("{\n");
				b.append("setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));\n");
				b.append("setBusy(false);\n\n");
				b.append("try\n");
				b.append("{\n");
				b.append("// We have to call method get() in order to see any exception!\n");
				b.append("get();\n");
				b.append("}\n");
				b.append("catch (final Exception ex)\n");
				b.append("{\n");
				b.append("if(ex instanceof InterruptedException)\n");
				b.append("Thread.currentThread().interrupt();\n\n");
				b.append("if(saveWorker.isCancelled())\n");
				b.append("return;\n\n");

				addErrorLog(b, "Error while performing save operation!", "ex");

				b.append("\n");
				b.append("JOptionPane.showMessageDialog(" + a.getForm().getName() + ".this, ");
				b.append(i18n.getI18NMessage("msg_err_save", "Error while performing save operation! Message: "));
				b.append(" + ex.getMessage(), ");
				b.append(i18n.getI18NMessage("msg_title_save", "Save operation") + ", JOptionPane.WARNING_MESSAGE);\n");
				b.append("return;\n");
				b.append("}\n\n");

				if (addToolBar) {
					b.append("// Enable all toolbar actions again!\n");
					b.append("for(int i = 0; i < toolBar.getComponentCount(); i++)\n");
					b.append("toolBar.getComponent(i).setEnabled(true);\n\n");
				}

				b.append("setStatusInfoMessage(");
				b.append(i18n.getI18NMessage("msg_status_save", "Save operation finished successfully!") + ");\n");
				b.append("setReturnCode(RETURN_CODE_OK);\n");
				b.append("dispose();\n");

				if ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && form.isOpenEditAfterCreate()) {
					for (final Form f : project.getAllFormsOfProject())
						if (f.getFormType() == FormTypeEnumeration.UPDATE && form.getDomainObject().equals(f.getDomainObject())) {
							final String getter = f.getDTO().getPKAttribute().getModelGetterName();

							b.append("\nnew " + f.getName() + "(" + form.getName() + ".this, ");
							b.append(resultObjName + "." + getter + ").setVisible(true);\n");
						}
				}

				b.append("}\n");
				b.append("};\n\n");
				b.append("saveWorker.execute();\n");
				b.append("}\n\n");
				break;
			}

		return b.toString();
	}

	/**
	 * @param invokeLater
	 * @param domainObject
	 * @return the generated content
	 */
	private String addDocumentFieldLogic(boolean invokeLater, DomainObject domainObject) {
		final var b = new StringBuilder();

		if (invokeLater) {
			b.append("SwingUtilities.invokeLater(() ->\n");
			b.append("{\n");
		}

		// Check if fields with appropriate tagging exist that should be filled automatically!
		for (final FormField f : form.getAllFormFields())
			if (f.getDTOAttribute().getDomainAttribute() != null
					&& f.getDTOAttribute().getDomainAttribute().getDomainObject().equals(domainObject)) {
				final DomainAttribute attr = f.getDTOAttribute().getDomainAttribute();

				if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_NAME) {
					if (f.isVisible())
						b.append(f.getName() + ".setText(new File(path).getName());\n");

					b.append(dto.getDomainObject().getLowerCaseName() + ".");
					b.append(f.getDTOAttribute().getModelSetterName() + "(new File(path).getName());\n");
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

		if (invokeLater)
			b.append("});\n\n");

		return b.toString();
	}

	/**
	 * @param panel
	 * @param layoutName
	 * @return the generated content for the GridBagLayout
	 */
	private String createGridBagLayout(FormPanel panel, String layoutName) {
		final var b = new StringBuilder();
		final var col1Map = new HashMap<Integer, FormField>();
		final var col2Map = new HashMap<Integer, FormField>();
		var rowHeights = layoutName + ".rowHeights = new int[]{";
		var rowWeights = layoutName + ".rowWeights = new double[]{";
		int rowCounter = 0;
		boolean usesSecondCol = false;

		// Sort all form fields of this panel
		ECollections.sort(panel.getFields(), (f1, f2) -> f1.getRowIndex() - f2.getRowIndex());

		// Initially the current row index is the row index of the first field!
		int currentRowIndex = panel.getFields().get(0).getRowIndex();

		// First we put every field into a separate column map
		for (final FormField f : panel.getFields()) {
			if (f.getRowIndex() != currentRowIndex)
				rowCounter++;

			currentRowIndex = f.getRowIndex();

			if (!f.isVisible()) {
				// If the field is not visible we put null into the map
				if (f.getColIndex() == 1)
					col1Map.put(rowCounter, null);
				else
					col2Map.put(rowCounter, null);
			}
			else {
				if (f.getColIndex() == 1)
					col1Map.put(rowCounter, f);
				else {
					col2Map.put(rowCounter, f);
					usesSecondCol = true;
				}

				if (f.isSpanCols())
					col2Map.put(rowCounter, f);
			}
		}

		// It is possible that there are some rows containing only invisible fields!
		for (int i = 0; i < col1Map.size(); i++) {
			final FormField fieldCol1 = col1Map.get(i);
			final FormField fieldCol2 = col2Map.get(i);

			if (fieldCol1 == null && fieldCol2 == null)
				continue;

			boolean multiLine = false;

			if (fieldCol1 != null && fieldCol2 == null) {
				if (fieldCol1.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL
						|| fieldCol1.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
					multiLine = true;
			}
			else if (fieldCol2.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL
					|| fieldCol2.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
				multiLine = true;

			if (multiLine) {
				rowWeights += "1.0,";
				rowHeights += "120,";
			}
			else {
				rowWeights += "0.0,";
				rowHeights += "0,";
			}
		}

		rowWeights += "Double.MIN_VALUE};\n";
		rowHeights += "0};\n";

		b.append("\nfinal var " + layoutName + " = new GridBagLayout();\n");

		if (usesSecondCol)
			b.append(layoutName + ".columnWeights = new double[]{0.0, 1.0, 0.0, 1.0};\n");
		else
			b.append(layoutName + ".columnWeights = new double[]{0.0, 1.0};\n");

		b.append(rowHeights);
		b.append(rowWeights);
		b.append("\n");
		b.append(panel.getName() + ".setLayout(" + layoutName + ");\n\n");

		return b.toString();
	}

	/**
	 * Add the panel to the form
	 * @param panel
	 * @param rowPanelCount
	 * @param columnsOfFirstRow
	 * @return the generated content
	 */
	private String addPanelToForm(FormPanel panel, int rowPanelCount, int columnsOfFirstRow) {
		final var b = new StringBuilder();
		boolean hasListField = false;
		boolean hasGrid = false;
		final boolean hasOneColumn = true;

		if (panel.getBasePanel() == null) {
			// Check if the panel provides a searchable list
			for (final FormField field : panel.getFields())
				if (field.getFieldType() == FormFieldTypeEnumeration.SEARCHABLE_LIST
						|| field.getFieldType() == FormFieldTypeEnumeration.LIST
						|| field.getFieldType() == FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR) {
					hasListField = true;
					break;
				}
		}
		else
			hasGrid = true;

		// If there is no data grid and no form fields we won't add the panel!
		if (!hasGrid && panel.getFields().isEmpty())
			return "";

		boolean addToTabFolder = false;

		if (panel.getRowIndex() == 1)
			if (rowPanelCount == 1)
				addToTabFolder = false;
			else
				addToTabFolder = true;
		else if (rowPanelCount > 1 || columnsOfFirstRow > 1)
			addToTabFolder = true;

		final var layoutName = "gbl" + panel.getName();

		b.append("final var " + panel.getName() + " = new JPanel();\n");
		b.append(panel.getName() + ".setBorder(new EmptyBorder(5, 5, 5, 5));\n");

		// We don't have to draw a border around the panel if it is placed onto a tab control!
		if (panel.isDrawBorder() && !addToTabFolder) {
			b.append(panel.getName() + ".setBorder(new TitledBorder(null, " + i18n.getI18N(panel));
			b.append(", TitledBorder.LEADING, TitledBorder.TOP, null, null));\n");
		}

		b.append("\n");

		if (addToTabFolder) {
			b.append("tabFolder" + panel.getRowIndex() + ".addTab(" + i18n.getI18N(panel));
			b.append(", null, " + panel.getName() + ", null);\n\n");
		}
		else {
			final var gbcPanelName = "gbc" + panel.getName();

			b.append("final var " + gbcPanelName + " = new GridBagConstraints();\n");
			b.append(gbcPanelName + ".insets = new Insets(5, 0, 0, 0);\n");
			b.append(gbcPanelName + ".fill = GridBagConstraints.BOTH;\n");
			b.append(gbcPanelName + ".gridx = 0;\n");

			// Check if a toolbar exists!
			final boolean toolBarExists = panel.getForm().getActions().stream().anyMatch(a -> a.getType() == ActionType.DOWNLOAD
					|| a.getType() == ActionType.INDIRECT_UPLOAD || a.getType() == ActionType.DIRECT_UPLOAD);

			if (toolBarExists)
				b.append(gbcPanelName + ".gridy = " + panel.getRowIndex() + ";\n");
			else
				b.append(gbcPanelName + ".gridy = " + (panel.getRowIndex() - 1) + ";\n");

			b.append("\n");
			b.append("panContent.add(" + panel.getName() + ", " + gbcPanelName + ");\n\n");
		}

		if (!hasGrid)
			if (hasListField)
				b.append(panel.getName() + ".setLayout(new BorderLayout(0, 0));\n\n");
			else
				b.append(createGridBagLayout(panel, layoutName));

		if (hasGrid) {
			final String tableName = TABLE_PREFIX + Integer.toString(panel.getRowIndex()) + Integer.toString(panel.getColIndex());
			final AbstractDomainAssociation association = panel.getBasePanel().getAssociation();

			b.append(panel.getName() + ".setLayout(new BorderLayout(0, 0));\n\n");

			if (association instanceof ManyToManyAssociation
					|| association instanceof final OneToManyAssociation otm && !otm.isBidirectional())
				b.append(tableName + " = new " + panel.getBasePanel().getName() + "(id);\n");
			else if (panel.getForm().getFormType() == FormTypeEnumeration.READONLY)
				b.append(tableName + " = new " + panel.getBasePanel().getName() + "(id, true);\n");
			else
				b.append(tableName + " = new " + panel.getBasePanel().getName() + "(id, false);\n");

			b.append("\n");
			b.append(panel.getName() + ".add(" + tableName + ", BorderLayout.CENTER);\n");
		}

		b.append("\n");

		if (!hasGrid) {
			// Sort all form fields of this panel
			ECollections.sort(panel.getFields(), new FormFieldComparator());

			// Add the form fields to the panel
			panel.getFields().forEach(field -> b
					.append(SwingFieldGeneratorFactory.getFieldGenerator(this, field, i18n).getFieldDefinitionFragment(hasOneColumn)));
		}

		return b.toString();
	}

}
