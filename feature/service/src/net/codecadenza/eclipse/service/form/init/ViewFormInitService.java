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
package net.codecadenza.eclipse.service.form.init;

import static net.codecadenza.eclipse.shared.Constants.ACTION_CREATE;
import static net.codecadenza.eclipse.shared.Constants.ACTION_DELETE;
import static net.codecadenza.eclipse.shared.Constants.ACTION_GET;
import static net.codecadenza.eclipse.shared.Constants.ACTION_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.ACTION_UPDATE;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import net.codecadenza.eclipse.model.boundary.BoundaryMethodDataFetchType;
import net.codecadenza.eclipse.model.client.ActionType;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormAction;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTable;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.client.TableColumnField;
import net.codecadenza.eclipse.model.client.TableColumnFieldTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.DomainTagEnumeration;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.dto.DtoFactory;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.Role;
import net.codecadenza.eclipse.service.form.init.util.AssociationHelper;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.ECollections;

/**
 * <p>
 * Utility class for initializing view forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ViewFormInitService {
	private static final String FORM_LOV_PREFIX = "";
	private static final String FORM_LOV_SUFFIX = "LOV";
	private static final String FORM_VIEW_SUFFIX = "View";
	private static final String FORM_TITLE_VIEW = " view";
	private static final String FORM_TITLE_LOV = "List of values for ";
	private static final String EJBQL_STD_TOKEN = "a.";
	private static final String PANEL_SUFFIX = "Panel";
	private static final String TABLE_SUFFIX = "Table";
	private static final int DEFAULT_FIELD_WIDTH_BOOLEAN = 70;
	private static final int DEFAULT_FIELD_WIDTH_STRING_SHORT = 80;
	private static final int DEFAULT_FIELD_WIDTH_STRING_MEDIUM = 100;
	private static final int DEFAULT_FIELD_WIDTH_STRING_LONG = 150;
	private static final int DEFAULT_FIELD_WIDTH_NUMBER = 80;
	private static final int DEFAULT_FIELD_WIDTH_DATE = 70;
	private static final int DEFAULT_FIELD_WIDTH_DATE_TIME = 120;
	private static final int DEFAULT_FIELD_WIDTH_UUID = 220;
	private static final int DEFAULT_MAX_FIELD_WIDTH = 150;
	private static final int WIDTH_FACTOR = 7;

	private final DomainObject domainObject;
	private final FormGroup formGroup;
	private final AssociationHelper rootAssociation;
	private final FormTypeEnumeration selType;
	private final BoundaryMethodDataFetchType dataFetchType;
	private final Project project;
	private final HashMap<String, DomainAttribute> downloadAttrMap = new HashMap<>();
	private Form form;
	private DTOBean dto;
	private boolean doEdit;

	/**
	 * Constructor
	 * @param domainObject
	 * @param formGroup
	 * @param selType
	 * @param dataFetchType
	 * @param rootAssociation
	 */
	public ViewFormInitService(DomainObject domainObject, FormGroup formGroup, FormTypeEnumeration selType,
			BoundaryMethodDataFetchType dataFetchType, AssociationHelper rootAssociation) {
		this.domainObject = domainObject;
		this.project = domainObject.getNamespace().getProject();
		this.formGroup = formGroup;
		this.selType = selType;
		this.dataFetchType = dataFetchType;
		this.rootAssociation = rootAssociation;
	}

	/**
	 * Constructor
	 * @param form
	 * @param rootAssociation
	 */
	public ViewFormInitService(Form form, AssociationHelper rootAssociation) {
		this(form.getDomainObject(), form.getFormGroup(), form.getFormType(), form.getBoundaryMethod().getDataFetchType(),
				rootAssociation);

		this.form = form;
		this.doEdit = true;
		this.dto = form.getDTO();
	}

	/**
	 * @return a map containing all attributes that represent the content for download operations
	 */
	public Map<String, DomainAttribute> getDownloadAttrMap() {
		return downloadAttrMap;
	}

	/**
	 * Initialize a view form and create the default table column fields
	 * @return the initialized form
	 */
	public Form initializeForm() {
		form = ClientFactory.eINSTANCE.createForm();
		form.setDomainObject(domainObject);
		form.setFormType(selType);

		if (selType == FormTypeEnumeration.LOV) {
			form.setName(FORM_LOV_PREFIX + domainObject.getName() + FORM_LOV_SUFFIX);
			form.setTitle(FORM_TITLE_LOV + domainObject.getLabelPlural());
		}
		else {
			form.setName(domainObject.getName() + FORM_VIEW_SUFFIX);
			form.setTitle(domainObject.getLabelPlural().substring(0, 1).toUpperCase() + domainObject.getLabelPlural().substring(1)
					+ FORM_TITLE_VIEW);
		}

		dto = DtoFactory.eINSTANCE.createDTOBean();
		dto.setDomainObject(domainObject);
		dto.setStandardConversion(false);
		dto.setComment("DTO for " + domainObject.getLabel().toLowerCase() + " objects");

		if (selType == FormTypeEnumeration.LOV)
			dto.setName(domainObject.getUpperCaseName() + "LovDTO");
		else if (selType == FormTypeEnumeration.SEARCHABLE_VIEW || selType == FormTypeEnumeration.SIMPLE_VIEW)
			dto.setName(domainObject.getUpperCaseName() + "SearchDTO");
		else
			dto.setName(domainObject.getUpperCaseName() + "ListDTO");

		final FormPanel formPanel = ClientFactory.eINSTANCE.createFormPanel();
		formPanel.setDTO(dto);
		formPanel.setName(domainObject.getName() + PANEL_SUFFIX);
		formPanel.setForm(form);

		final FormTable formTable = ClientFactory.eINSTANCE.createFormTable();
		formTable.setFormPanel(formPanel);
		formTable.setName(domainObject.getName() + TABLE_SUFFIX);
		formPanel.setFormTable(formTable);

		form.getFormPanels().add(formPanel);
		form.setDTO(dto);
		form.getRoles().addAll(formGroup.getRoles());

		// Add the default columns
		for (final DomainAttribute attr : domainObject.getAllAttributes()) {
			boolean isPK = false;

			if (attr.isPk())
				isPK = true;

			// Non-persistent columns cannot be supported due to the constructor conversion of DTOs!
			if (attr.isTrackVersion() || !attr.isPersistent())
				continue;

			// Add the attribute as a table column field
			addTableColumn(attr, EJBQL_STD_TOKEN + attr.getName(), isPK, false, true, null, -1);
		}

		// Add columns of the first-level associations
		for (int i = 0; i < 2; i++)
			for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
				// Do not add items if the association is bidirectional to the parent
				if (domainObject.equals(assoc.getTarget()))
					continue;

				// Do not cover one-to-many and many-to-many associations by default!
				if (assoc instanceof OneToManyAssociation || assoc instanceof ManyToManyAssociation)
					continue;

				// Test if the association is optional
				if (assoc instanceof final OneToOneAssociation oneToOne && oneToOne.isOptional())
					continue;

				if (assoc instanceof final ManyToOneAssociation manyToOne && manyToOne.isOptional())
					continue;

				for (final DomainObject assocBean : assoc.getTarget().getFullInheritanceTree()) {
					if (selType != FormTypeEnumeration.LOV) {
						if (dataFetchType == BoundaryMethodDataFetchType.CLIENT && assoc.getTarget().getTag() == DomainTagEnumeration.CLIENT)
							continue;

						if (dataFetchType == BoundaryMethodDataFetchType.USER && assoc.getTarget().getTag() == DomainTagEnumeration.USER)
							continue;
					}

					for (final DomainAttribute attr : assocBean.getAttributes()) {
						final boolean isPK = attr.isPk();
						boolean isVisible = true;

						// Check if a unique key exists
						if (assoc instanceof ManyToOneAssociation) {
							if (!isPK && !attr.isDisplayAttribute())
								continue;

							if (isPK && attr.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE)
								isVisible = false;
						}
						else if (assoc instanceof OneToOneAssociation
								&& (attr.isTrackVersion() || attr.isSetDateOnPersist() || attr.isSetDateOnUpdate()))
							continue;

						final AssociationHelper assocHelper = rootAssociation.searchAssociationHelper(assoc);

						// Add all visible table columns first. In a second run we add the invisible columns.
						if ((isVisible && i == 0) || (!isVisible && i == 1))
							addTableColumn(attr, assocHelper.getAlias() + "." + attr.getName(), false, false, isVisible, Arrays.asList(assoc),
									-1);
					}
				}
			}

		// A list-of-values form should not be supplied with default actions!
		if (selType != FormTypeEnumeration.LOV)
			addDefaultActions();

		return form;
	}

	/**
	 * Generate the select statement
	 * @param skipSelectClause
	 * @return the generated string
	 */
	public String generateSelectStatement(boolean skipSelectClause) {
		final var b = new StringBuilder();
		final var aliasMap = new HashMap<String, String>();
		final var recursionQueryPart = "";

		// We just return an empty string if no attribute exists yet!
		if (dto.getAttributes().isEmpty())
			return "";

		if (!skipSelectClause) {
			boolean firstAttribute = true;
			b.append("select\n");

			for (final DTOBeanAttribute attr : dto.getAttributes()) {
				if (firstAttribute)
					firstAttribute = false;
				else
					b.append(",\n");

				b.append(attr.getSelectToken());

				final String token = attr.getSelectToken().substring(0, attr.getSelectToken().indexOf('.'));

				// Extract the alias in order to put it into the map
				if (!token.equals(AssociationHelper.INITIAL_ALIAS))
					aliasMap.put(token, "");
			}

			b.append("\n");
		}
		else {
			dto.getAttributes().forEach(attr -> {
				final String token = attr.getSelectToken().substring(0, attr.getSelectToken().indexOf('.'));

				if (!token.equals(AssociationHelper.INITIAL_ALIAS))
					aliasMap.put(token, "");
			});
		}

		// The automatic select statement generation is not possible in edit mode!
		if (doEdit) {
			b.append(form.getBoundaryMethod().getQueryStatement());

			return b.toString();
		}

		b.append("from ");
		b.append(domainObject.getName());
		b.append(" a");

		final var aliasCollection = new BasicEList<String>();

		aliasMap.keySet().forEach(aliasCollection::add);

		// Sort the aliases
		ECollections.sort(aliasCollection, String::compareTo);

		final var usedJoins = new HashSet<String>();

		// Add join statements for all associations!
		aliasCollection.forEach(alias -> b.append(rootAssociation.generateJoinStatement(alias, usedJoins, false)));

		b.append(recursionQueryPart);

		return b.toString();
	}

	/**
	 * Add a table column field
	 * @param attribute
	 * @param selectName
	 * @param isPK
	 * @param isRefIdentifier
	 * @param visible
	 * @param assocList
	 * @param colIndex
	 * @return the new table column field
	 */
	public TableColumnField addTableColumn(DomainAttribute attribute, String selectName, boolean isPK, boolean isRefIdentifier,
			boolean visible, List<AbstractDomainAssociation> assocList, int colIndex) {
		final FormTable formTable = form.getFormPanels().get(0).getFormTable();
		var title = "";

		if (attribute.isLob())
			return null;

		// Check if the field is already exists!
		for (final TableColumnField col : formTable.getFields())
			if (col.getDTOAttribute().getSelectToken().equals(selectName))
				return null;

		// Initialize the table column field
		final TableColumnField col = ClientFactory.eINSTANCE.createTableColumnField();
		col.setWidth(100);
		col.setIdentifier(isPK);

		final DTOBeanAttribute attr = dto.addAttribute(attribute, assocList, true);
		attr.setSelectToken(selectName);

		if (assocList != null && !assocList.isEmpty()) {
			final var b = new StringBuilder();

			for (final char c : attr.getName().toCharArray())
				if (c == Character.toUpperCase(c))
					b.append(" " + Character.toLowerCase(c));
				else
					b.append(c);

			title = b.toString().trim();
			title = title.substring(0, 1).toUpperCase() + title.substring(1);
		}
		else
			title = attr.getDomainAttribute().getLabel().substring(0, 1).toUpperCase()
					+ attr.getDomainAttribute().getLabel().substring(1);

		// Set the list-of-values return value
		if (selType == FormTypeEnumeration.LOV && !doEdit && domainObject.getDisplayAttribute() != null
				&& domainObject.getDisplayAttribute().equals(attribute))
			attr.setLovReturn(true);

		if (colIndex == -1)
			col.setColIndex(formTable.getFields().size() + 1);
		else
			col.setColIndex(colIndex);

		col.setTitle(title);
		col.setFormTable(formTable);
		col.setVisible(visible);
		col.setAssociationRef(isRefIdentifier);
		col.setDTOAttribute(attr);
		col.setSearchable(attribute.isPersistent());

		if (attribute.getJavaType().isBoolean()) {
			col.setWidth(DEFAULT_FIELD_WIDTH_BOOLEAN);
			col.setFieldType(TableColumnFieldTypeEnumeration.BOOLEAN);
		}
		else if (attribute.getJavaType().isString()) {
			if (attribute.isPersistent()) {
				if (attribute.getColumn().getLength() < 50)
					col.setWidth(DEFAULT_FIELD_WIDTH_STRING_SHORT);
				else if (attribute.getColumn().getLength() < 100)
					col.setWidth(DEFAULT_FIELD_WIDTH_STRING_MEDIUM);
				else
					col.setWidth(DEFAULT_FIELD_WIDTH_STRING_LONG);
			}
			else
				col.setWidth(DEFAULT_FIELD_WIDTH_STRING_MEDIUM);

			col.setFieldType(TableColumnFieldTypeEnumeration.STRING);
		}
		else if (attribute.getJavaType().isChar()) {
			col.setWidth(DEFAULT_FIELD_WIDTH_STRING_SHORT);
			col.setFieldType(TableColumnFieldTypeEnumeration.CHAR);
		}
		else if (attribute.getJavaType().isInteger()) {
			if (isPK || col.isAssociationRef())
				col.setVisible(false);

			col.setWidth(DEFAULT_FIELD_WIDTH_NUMBER);
			col.setFieldType(TableColumnFieldTypeEnumeration.INTEGER);
		}
		else if (attribute.getJavaType().isLong()) {
			if (isPK || col.isAssociationRef())
				col.setVisible(false);

			col.setWidth(DEFAULT_FIELD_WIDTH_NUMBER);
			col.setFieldType(TableColumnFieldTypeEnumeration.LONG);
		}
		else if (attribute.getJavaType().isDouble()) {
			col.setWidth(DEFAULT_FIELD_WIDTH_NUMBER);
			col.setFieldType(TableColumnFieldTypeEnumeration.DOUBLE);
		}
		else if (attribute.getJavaType().isBigDecimal()) {
			col.setWidth(DEFAULT_FIELD_WIDTH_NUMBER);
			col.setFieldType(TableColumnFieldTypeEnumeration.BIG_DECIMAL);
		}
		else if (attribute.getJavaType().isFloat()) {
			col.setWidth(DEFAULT_FIELD_WIDTH_NUMBER);
			col.setFieldType(TableColumnFieldTypeEnumeration.FLOAT);
		}
		else if (attribute.getJavaType().isDateOrCalendar()) {
			if (attribute.getTemporalType() == TemporalTypeEnumeration.DATE)
				col.setWidth(DEFAULT_FIELD_WIDTH_DATE);
			else
				col.setWidth(DEFAULT_FIELD_WIDTH_DATE_TIME);

			if (attribute.getJavaType().isDate())
				col.setFieldType(TableColumnFieldTypeEnumeration.DATE);
			else
				col.setFieldType(TableColumnFieldTypeEnumeration.GREGORIAN_CALENDAR);
		}
		else if (attribute.getJavaType().isLocalDate()) {
			col.setWidth(DEFAULT_FIELD_WIDTH_DATE);
			col.setFieldType(TableColumnFieldTypeEnumeration.LOCAL_DATE);
		}
		else if (attribute.getJavaType().isLocalDateTime()) {
			col.setWidth(DEFAULT_FIELD_WIDTH_DATE_TIME);
			col.setFieldType(TableColumnFieldTypeEnumeration.LOCAL_DATE_TIME);
		}
		else if (attribute.getJavaType().isUUID()) {
			col.setWidth(DEFAULT_FIELD_WIDTH_UUID);

			if (attribute.isWildcardFilteringSupported())
				col.setFieldType(TableColumnFieldTypeEnumeration.UUID_STRING);
			else
				col.setFieldType(TableColumnFieldTypeEnumeration.UUID_BINARY);
		}
		else {
			col.setWidth(DEFAULT_FIELD_WIDTH_STRING_MEDIUM);
			col.setFieldType(TableColumnFieldTypeEnumeration.ENUM);
		}

		// Correct the default field width if the title needs more space
		if (col.getWidth() < DEFAULT_MAX_FIELD_WIDTH) {
			final int newWidth = title.length() * WIDTH_FACTOR;

			if (newWidth > col.getWidth())
				col.setWidth(newWidth);
		}

		if (colIndex != -1) {
			// Change the column index of all following columns
			for (final TableColumnField c : formTable.getFields())
				if (c.getColIndex() >= colIndex && !c.equals(col))
					c.setColIndex(c.getColIndex() + 1);
		}

		formTable.getFields().add(col);

		return col;
	}

	/**
	 * Add the actions to the view
	 */
	private void addDefaultActions() {
		final FormAction deleteAction = ClientFactory.eINSTANCE.createFormAction();
		deleteAction.setName(ACTION_DELETE);
		deleteAction.setDescription("Delete selected " + domainObject.getLabel());
		deleteAction.setType(ActionType.DELETE);

		// Add the default roles to the delete action!
		for (final Role role : project.getRoles())
			if (role.isAdminRole())
				deleteAction.getRoles().add(role);

		form.getActions().add(deleteAction);

		final var formsOfDomainObject = new BasicEList<Form>();

		for (final Form f : project.getAllFormsOfProject())
			if (f.getDomainObject().equals(domainObject)) {
				final FormTypeEnumeration formType = f.getFormType();

				if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY
						|| formType == FormTypeEnumeration.CREATE)
					formsOfDomainObject.add(f);
			}

		formsOfDomainObject.forEach(f -> {
			final FormAction action = ClientFactory.eINSTANCE.createFormAction();

			// Add the default roles to this action!
			action.getRoles().addAll(f.getRoles());

			if (f.getFormType() == FormTypeEnumeration.CREATE) {
				action.setDescription("Create new " + f.getDomainObject().getLabel());
				action.setName(ACTION_CREATE);
				action.setType(ActionType.CREATE);
			}
			else if (f.getFormType() == FormTypeEnumeration.UPDATE) {
				action.setDescription("Edit " + f.getDomainObject().getLabel());
				action.setName(ACTION_UPDATE);
				action.setType(ActionType.UPDATE);
			}
			else if (f.getFormType() == FormTypeEnumeration.READONLY) {
				action.setDescription("View " + f.getDomainObject().getLabel());
				action.setName(ACTION_GET);
				action.setType(ActionType.READ);
			}

			action.setTargetForm(f);

			form.getActions().add(action);
		});

		// Add an action to perform a deep copy of the selected object! This is only supported for non-abstract entities!
		if (!domainObject.isAbstract()) {
			final var actionName = ACTION_PREFIX + "Copy";
			final FormAction copyAction = ClientFactory.eINSTANCE.createFormAction();

			// Add all roles that have have the administrator flag set
			for (final Role role : project.getRoles())
				if (role.isAdminRole())
					copyAction.getRoles().add(role);

			copyAction.setDescription("Create copy of selected " + domainObject.getLabel());
			copyAction.setName(actionName);
			copyAction.setType(ActionType.COPY);

			form.getActions().add(copyAction);
		}

		// Add all actions of LOB attributes
		domainObject.getAllLobAttributes().forEach(attr -> {
			var actionName = ACTION_PREFIX + attr.getUpperCaseName() + "Download";

			if (!attr.getDomainObject().equals(domainObject))
				actionName = ACTION_PREFIX + attr.getDomainObject().getUpperCaseName() + attr.getUpperCaseName() + "Download";

			// Add an action to download the data
			final FormAction downloadAction = ClientFactory.eINSTANCE.createFormAction();

			// Add all available roles to this action!
			downloadAction.getRoles().addAll(project.getRoles());

			downloadAction.setDescription("Download " + attr.getLabel());
			downloadAction.setName(actionName);
			downloadAction.setType(ActionType.DOWNLOAD);

			form.getActions().add(downloadAction);

			downloadAttrMap.put(downloadAction.getName(), attr);
		});
	}

}
