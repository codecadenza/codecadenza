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

import static net.codecadenza.eclipse.shared.Constants.ADMIN_PANEL_TITLE;
import static net.codecadenza.eclipse.shared.Constants.BASIC_PANEL_TITLE;
import static net.codecadenza.eclipse.shared.Constants.EDITOR_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.CHECKBOX_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.COMBO_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.CREATE_DTO_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.DEFAULT_VALUE_FLAG;
import static net.codecadenza.eclipse.shared.Constants.DTO_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.INITIAL_ONE_TO_MANY_DTO_SUFFIX;
import static net.codecadenza.eclipse.shared.Constants.LIST_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.TEXT_PREFIX;
import static net.codecadenza.eclipse.shared.Constants.UPDATE_DTO_SUFFIX;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.model.client.ClientFactory;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.client.FormField;
import net.codecadenza.eclipse.model.client.FormFieldTypeEnumeration;
import net.codecadenza.eclipse.model.client.FormGroup;
import net.codecadenza.eclipse.model.client.FormPanel;
import net.codecadenza.eclipse.model.client.FormTypeEnumeration;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.AssociationTagEnumeration;
import net.codecadenza.eclipse.model.domain.AttributeTagEnumeration;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainAttributeValidator;
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
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.service.dto.DTOBeanService;
import net.codecadenza.eclipse.service.form.init.util.FormLayoutOptimizer;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;

/**
 * <p>
 * Utility class for initializing single-record forms
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class UpdateFormInitService {
	private static final String FORM_CREATE_PREFIX = "CreateNew";
	private static final String FORM_CREATE_SUFFIX = "Dialog";
	private static final String FORM_EDIT_PREFIX = "Edit";
	private static final String FORM_EDIT_SUFFIX = "Dialog";
	private static final String FORM_READONLY_PREFIX = "View";
	private static final String FORM_READONLY_SUFFIX = "Dialog";
	private static final String FORM_ADD_PREFIX = "Add";
	private static final String FORM_ADD_SUFFIX = "Dialog";
	private static final String FORM_TITLE_CREATE = "Create new ";
	private static final String FORM_TITLE_ADD = "Add new ";
	private static final String FORM_TITLE_EDIT = "Edit ";
	private static final String FORM_TITLE_READONLY = "View ";
	private static final String PANEL_PREFIX = "pan";
	private static final int TEXT_FIELD_HEIGHT = 25;
	private static final int MULTI_TEXT_FIELD_HEIGHT = 150;
	private static final int MIN_SIMPLE_FORM_WIDTH = 350;
	private static final int MIN_SIMPLE_FORM_HEIGHT = 230;
	private static final int MAX_FORM_HEIGHT = 768;
	private static final int MAX_FORM_WIDTH = 800;
	private static final int MIN_FORM_HEIGHT_WITH_ADMIN_PANEL = 270;
	private static final double FORM_WIDTH_FACTOR = 1.2;
	private static final int DEFAULT_FORM_HEIGHT = 200;
	private static final int SMALL_TEXT_SIZE = 50;
	private static final int MEDIUM_TEXT_SIZE = 100;
	private static final int LONG_TEXT_SIZE = 512;
	private static final int UUID_TEXT_SIZE = 250;
	private static final Pattern PANEL_PREFIX_PATTERN = Pattern.compile(PANEL_PREFIX);

	private final DomainObject domainObject;
	private final FormGroup formGroup;
	private final FormTypeEnumeration formType;
	private final Project project;
	private final boolean addSecurity;
	private Form form;
	private final boolean shareDTO;
	private HashMap<String, DTOBean> listDTOMap = new HashMap<>();
	private HashMap<String, DTOBean> addDTOMap = new HashMap<>();
	private HashMap<DTOBean, Namespace> namespaceMap = new HashMap<>();

	/**
	 * Constructor
	 * @param domainObject
	 * @param formGroup
	 * @param formType
	 * @param shareDTO
	 */
	public UpdateFormInitService(DomainObject domainObject, FormGroup formGroup, FormTypeEnumeration formType, boolean shareDTO) {
		this.project = domainObject.getNamespace().getProject();
		this.domainObject = domainObject;
		this.addSecurity = project.getApplicationLogOnDTO() != null;
		this.formType = formType;
		this.formGroup = formGroup;
		this.shareDTO = shareDTO;
	}

	/**
	 * Constructor
	 * @param form
	 */
	public UpdateFormInitService(Form form) {
		this(form.getDomainObject(), form.getFormGroup(), form.getFormType(), form.getDTO().isShared());

		this.form = form;
	}

	/**
	 * @return a map containing all additional DTOs. The respective boundary parameter name represents the key.
	 */
	public Map<String, DTOBean> getAddDTOMap() {
		return addDTOMap;
	}

	/**
	 * @return a map containing all list DTOs. The name of the target domain object represents the key.
	 */
	public Map<String, DTOBean> getListDTOMap() {
		return listDTOMap;
	}

	/**
	 * @return a map containing all DTOs and the respective namespace they belong to
	 */
	public Map<DTOBean, Namespace> getNamespaceMap() {
		return namespaceMap;
	}

	/**
	 * @return the initialized form
	 */
	public Form initializeForm() {
		createDefaultForm();

		return form;
	}

	/**
	 * @return true if the form is opened in a new window
	 */
	public boolean isOpenedInNewWindow() {
		return !project.hasAngularClient() && !project.hasJSFOrVaadinClient();
	}

	/**
	 * Create a panel for an initial one-to-many association object
	 * @param assoc
	 * @return the initialized form panel
	 */
	public FormPanel createInitialOneToManyPanel(OneToManyAssociation assoc) {
		final DomainObject bean = assoc.getTarget();
		final ManyToOneAssociation mto = assoc.getReverseAssociation();
		final String label = bean.getLabel();
		final var panelLabel = "Initial " + label;

		// Set a proper panel name. Note that it is very important to set the name of the panel in this way as further operations
		// expect this format!
		String panelName = assoc.getUpperCaseName();

		if (panelName.endsWith("s"))
			panelName = panelName.substring(0, panelName.length() - 1);
		else if (panelName.endsWith("ies"))
			panelName = panelName.substring(0, panelName.length() - 3) + "y";

		final FormPanel oneToManyPanel = ClientFactory.eINSTANCE.createFormPanel();
		oneToManyPanel.setName(PANEL_PREFIX + panelName);
		oneToManyPanel.setLabel(panelLabel);
		oneToManyPanel.setColIndex(form.getFormPanels().size() + 1);
		oneToManyPanel.setForm(form);

		form.getFormPanels().add(oneToManyPanel);

		// Create the DTO for this panel
		final DTOBean panelDTO = DtoFactory.eINSTANCE.createDTOBean();
		panelDTO.setDomainObject(bean);
		panelDTO.setPrimitive(false);
		panelDTO.setMappable(false);
		panelDTO.setStandardConversion(true);
		panelDTO.setComment("Data transfer object for " + bean.getLabel() + " objects");
		panelDTO.setName(bean.getName() + INITIAL_ONE_TO_MANY_DTO_SUFFIX);

		oneToManyPanel.setDTO(panelDTO);

		String paramName = PANEL_PREFIX_PATTERN.matcher(panelName).replaceFirst("");
		paramName = paramName.substring(0, 1).toLowerCase() + paramName.substring(1);

		addDTOMap.put(paramName, panelDTO);

		for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
			if (ns.getName().equals(bean.getNamespace().getName())) {
				namespaceMap.put(panelDTO, ns);
				break;
			}

		// Add fields that are mapped to attributes
		for (final DomainAttribute attr : bean.getAllAttributes()) {
			if (!attr.isPersistent())
				continue;

			addFormField(panelDTO, oneToManyPanel, attr, panelName, !attr.isPersistent(), null);
		}

		// Add fields that are mapped to many-to-one associations
		for (final AbstractDomainAssociation a : bean.getAllAssociations())
			if (a instanceof final ManyToOneAssociation manyToOne) {
				// Do not add the reverse field of this association
				if (a.equals(mto))
					continue;

				if (manyToOne.isInsertable())
					addFormList(panelDTO, oneToManyPanel, Arrays.asList(manyToOne));
			}

		return oneToManyPanel;
	}

	/**
	 * Add a list component to the panel in order to cover many-to-many and many-to-one relationships
	 * @param dto
	 * @param panel
	 * @param assocList
	 * @return the new form field
	 */
	public FormField addFormList(DTOBean dto, FormPanel panel, List<AbstractDomainAssociation> assocList) {
		final AbstractDomainAssociation assoc = assocList.get(0);
		DomainAttribute displayAttribute = null;

		// Check if the list DTO already exists!
		DTOBean listDTO = listDTOMap.get(assoc.getTarget().getName());

		if (listDTO == null) {
			listDTO = new DTOBeanService(project).getOrCreateListDTO(assoc.getTarget());

			// Set the namespace of the DTO
			for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
				if (ns.getName().equals(assoc.getTarget().getNamespace().getName())) {
					namespaceMap.put(listDTO, ns);
					break;
				}

			listDTOMap.put(assoc.getTarget().getName(), listDTO);
		}

		final DTOBeanAttribute dtoAttribute = dto.addAttribute(listDTO, null, assocList, true);

		if (listDTO.getDisplayAttribute() != null)
			displayAttribute = listDTO.getDisplayAttribute().getDomainAttribute();

		final FormField field = ClientFactory.eINSTANCE.createFormField();
		field.setRowIndex(panel.getFields().size() + 1);
		field.setDTOAttribute(dtoAttribute);
		field.setPanel(panel);

		field.setLabel(assoc.getGUILabel());
		panel.getFields().add(field);

		if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation) {
			field.setName(LIST_PREFIX + dtoAttribute.getUpperCaseName());
			field.setFieldType(FormFieldTypeEnumeration.SEARCHABLE_LIST);
		}
		else {
			boolean fieldTypeSet = false;
			final var mto = (ManyToOneAssociation) assoc;

			field.setMandatory(!mto.isOptional());
			field.setName(COMBO_PREFIX + dtoAttribute.getUpperCaseName());

			// Check if the reference can be set by the application's security service!
			if (addSecurity && ((formType == FormTypeEnumeration.UPDATE && mto.isUpdatable() && !mto.isInsertable())
					|| ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && !mto.isUpdatable()
							&& mto.isInsertable()))) {
				final DomainTagEnumeration tag = field.getDTOAttribute().getReferencedDTOBean().getDomainObject().getTag();

				if (tag == DomainTagEnumeration.USER) {
					fieldTypeSet = true;
					field.setFieldType(FormFieldTypeEnumeration.SELECTION_BY_SECURITY_DTO);
					field.setVisible(false);
				}
			}

			// If the many-to-one association belongs to a one-to-many association it is likely that this field can be set by a parent
			// form!
			if (!fieldTypeSet && formType == FormTypeEnumeration.ADD && !mto.isOwner()) {
				fieldTypeSet = true;
				field.setFieldType(FormFieldTypeEnumeration.SELECTION_BY_PARENT_FORM);
				field.setVisible(false);
			}

			if (!fieldTypeSet) {
				if (formType == FormTypeEnumeration.READONLY || (formType == FormTypeEnumeration.UPDATE && !mto.isUpdatable())) {
					field.setFieldType(FormFieldTypeEnumeration.FORM_LINK);
					field.setReadonly(true);
				}
				else {
					// Search for a list-of-values form which can be used for this field!
					final Form lovForm = field.findListOfValues();

					if (lovForm != null) {
						field.setFieldType(FormFieldTypeEnumeration.LOV);
						field.setListOfValues(lovForm);
					}
					else
						field.setFieldType(FormFieldTypeEnumeration.PROPOSAL_TEXT);

					if (project.isMandatingSupported() && mto.getTag() == AssociationTagEnumeration.CLIENT_REFERENCE) {
						field.setFieldType(FormFieldTypeEnumeration.SELECTION_BY_CLIENT);
						field.setVisible(false);
					}
				}
			}
		}

		DomainAttribute attr = displayAttribute;

		if (attr == null)
			attr = listDTO.getPKAttribute().getDomainAttribute();

		field.setWidth(120);

		// Calculate the field width
		if (assoc instanceof ManyToOneAssociation) {
			final int colWidth = attr.getColumn().getLength();

			if (colWidth >= MEDIUM_TEXT_SIZE)
				field.setSpanCols(true);

			if (colWidth < SMALL_TEXT_SIZE)
				field.setWidth(100);
		}

		return field;
	}

	/**
	 * Add a form field to the panel
	 * @param dto
	 * @param panel
	 * @param attr
	 * @param fieldNamePrefix
	 * @param forceReadonly
	 * @param assocList
	 * @return the new form field
	 */
	public FormField addFormField(DTOBean dto, FormPanel panel, DomainAttribute attr, String fieldNamePrefix, boolean forceReadonly,
			List<AbstractDomainAssociation> assocList) {
		final AbstractDomainAssociation assoc = assocList != null && !assocList.isEmpty() ? assocList.get(0) : null;

		if ((formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD) && attr.isSetDateOnUpdate())
			return null;

		// We do not handle LOB attributes as form fields
		if ((formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY) && attr.isLob())
			return null;

		// We do not create a field if the attribute represents a document reference
		if ((formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
				&& attr.getTag() == AttributeTagEnumeration.DOCUMENT_REF)
			return null;

		String dtoAttributeName = attr.getName();

		if (!fieldNamePrefix.isEmpty()) {
			final String prefix = fieldNamePrefix.substring(0, 1).toLowerCase() + fieldNamePrefix.substring(1);
			dtoAttributeName = prefix + attr.getUpperCaseName();
		}

		final String fieldName = fieldNamePrefix + attr.getUpperCaseName();
		final DTOBeanAttribute dtoAttribute = dto.addAttribute(attr, dtoAttributeName, assocList, true);

		// We do not handle LOB attributes as form fields but in this case we need the DTO attribute!
		if ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && attr.isLob())
			return null;

		// In case of document references we also do not need a dedicated field!
		if ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE)
				&& attr.getTag() == AttributeTagEnumeration.DOCUMENT_REF)
			return null;

		final FormField field = ClientFactory.eINSTANCE.createFormField();
		field.setDTOAttribute(dtoAttribute);

		if (attr.getCollectionType() == CollectionTypeEnumeration.NONE) {
			// Check if the attribute represents an auto-incremented primary key
			if (attr.isPk() && attr.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE
					&& (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD))
				field.setVisible(false);
			else
				field.setVisible(!((formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD)
						&& (attr.isTrackVersion() || attr.isSetDateOnPersist())));

			if (attr.getJavaType().isBoolean()) {
				field.setName(CHECKBOX_PREFIX + fieldName);
				field.setFieldType(FormFieldTypeEnumeration.CHECKBOX);

				if (formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE)
					field.setDefaultValue(DEFAULT_VALUE_FLAG);
			}
			else if (attr.getJavaType().isNumber() || attr.getJavaType().isChar()) {
				field.setWidth(120);
				field.setName(TEXT_PREFIX + fieldName);

				if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_SIZE)
					field.setFieldType(FormFieldTypeEnumeration.DOCUMENT_SIZE_FIELD);
				else
					field.setFieldType(FormFieldTypeEnumeration.SIMPLE_TEXT);
			}
			else if (attr.getJavaType().isTemporalType()) {
				if (attr.getJavaType().isLocalDate() || attr.getTemporalType() == TemporalTypeEnumeration.DATE) {
					field.setWidth(100);
					field.setFieldType(FormFieldTypeEnumeration.DATE);
				}
				else {
					field.setWidth(120);
					field.setFieldType(FormFieldTypeEnumeration.DATE_TIME);
				}

				field.setName(TEXT_PREFIX + fieldName);

				if ((formType == FormTypeEnumeration.ADD || formType == FormTypeEnumeration.CREATE) && !attr.isSetDateOnPersist())
					field.setDefaultValue(DEFAULT_VALUE_FLAG);
			}
			else if (attr.getJavaType().isString()) {
				field.setName(TEXT_PREFIX + fieldName);

				final DomainAttributeValidator validator = attr.getDomainAttributeValidator();

				field.setFieldType(FormFieldTypeEnumeration.SIMPLE_TEXT);
				final int stringLength = validator.getMaxLength() == null ? LONG_TEXT_SIZE : validator.getMaxLength();

				if (stringLength >= LONG_TEXT_SIZE) {
					field.setSpanCols(true);
					field.setFieldType(FormFieldTypeEnumeration.MULTI_LINE_TEXT);
				}
				else if (stringLength > MEDIUM_TEXT_SIZE)
					field.setSpanCols(true);
				else if (stringLength < SMALL_TEXT_SIZE)
					field.setWidth(100);
				else
					field.setWidth(140);
			}
			else if (attr.getJavaType().isUUID()) {
				field.setWidth(UUID_TEXT_SIZE);
				field.setName(TEXT_PREFIX + fieldName);
				field.setFieldType(FormFieldTypeEnumeration.SIMPLE_TEXT);
			}
			else {
				field.setName(COMBO_PREFIX + fieldName);
				field.setFieldType(FormFieldTypeEnumeration.ENUM_COMBOBOX);
				field.setWidth(140);
			}
		}
		else {
			field.setName(EDITOR_PREFIX + fieldName);
			field.setFieldType(FormFieldTypeEnumeration.ELEMENT_COLLECTION_EDITOR);
			field.setSpanCols(true);
			field.setWidth(300);
		}

		if (forceReadonly || (formType == FormTypeEnumeration.READONLY))
			field.setReadonly(true);
		else {
			field.setReadonly(false);

			// It should not be possible to edit the value of a primary key field
			if (attr.isPk() && formType == FormTypeEnumeration.UPDATE)
				field.setReadonly(true);

			if (!attr.isUpdatable() && formType == FormTypeEnumeration.UPDATE)
				field.setReadonly(true);

			// A version field should not be editable!
			if (attr.isTrackVersion())
				field.setReadonly(true);

			// Fields for creation date and last update are not editable
			if (attr.isSetDateOnPersist() || attr.isSetDateOnUpdate())
				field.setReadonly(true);

			// In case of tagging some special fields should be set to be read-only by default
			if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_NAME)
				field.setReadonly(true);

			if (attr.getTag() == AttributeTagEnumeration.DOCUMENT_SIZE)
				field.setReadonly(true);
		}

		field.setLabel(attr.getGUILabel());
		field.setColIndex(1);
		field.setRowIndex(panel.getFields().size() + 1);
		field.setPanel(panel);

		boolean checkOneToOneAttr = false;

		if (assoc != null) {
			if (assoc instanceof final ManyToOneAssociation manyToOne)
				field.setMandatory(!manyToOne.isOptional());

			if (assoc instanceof OneToOneAssociation)
				checkOneToOneAttr = true;
		}

		if (assoc == null || checkOneToOneAttr) {
			field.setMandatory(false);

			// Every field that is mapped to a primitive type is mandatory!
			if (attr.getJavaType().isPrimitive()) {
				// A field that is mapped to an attribute of type char is not mandatory if the minimum length is 0!
				field.setMandatory(!(attr.getJavaType().isChar() && attr.getMinFieldLength().isEmpty()));
			}
			else if (attr.getJavaType().isString()) {
				// Every field that is mapped to an object type is mandatory if the attribute is not nullable! A field of java.lang.String
				// is mandatory if the minimum field length is greater than 0! We don't care if the field is nullable or not!
				field.setMandatory(attr.getMinFieldLength().isPresent());
			}
			else if (!attr.getDomainAttributeValidator().isNullable())
				field.setMandatory(true);
		}

		panel.getFields().add(field);
		return field;
	}

	/**
	 * Create the default form
	 */
	private void createDefaultForm() {
		int panelColIndex1 = 1;
		int panelColIndex2 = 1;
		boolean hasSearchableListPanel = false;
		boolean hasTablePanel = false;
		boolean hasAdminPanel = false;
		boolean hasElementCollectionPanel = false;
		namespaceMap = new HashMap<>();
		listDTOMap = new HashMap<>();
		addDTOMap = new HashMap<>();
		FormPanel adminPanel = null;

		form = ClientFactory.eINSTANCE.createForm();
		form.setDomainObject(domainObject);
		form.setFormType(formType);

		final FormPanel formPanel = ClientFactory.eINSTANCE.createFormPanel();
		formPanel.setName(PANEL_PREFIX + domainObject.getName());
		formPanel.setLabel(BASIC_PANEL_TITLE);
		formPanel.setColIndex(panelColIndex1++);

		form.getFormPanels().add(formPanel);
		formPanel.setForm(form);

		if (formType == FormTypeEnumeration.CREATE) {
			form.setName(FORM_CREATE_PREFIX + domainObject.getName() + FORM_CREATE_SUFFIX);
			form.setTitle(FORM_TITLE_CREATE + domainObject.getLabel());
		}
		else if (formType == FormTypeEnumeration.ADD) {
			form.setName(FORM_ADD_PREFIX + domainObject.getName() + FORM_ADD_SUFFIX);
			form.setTitle(FORM_TITLE_ADD + domainObject.getLabel());
		}
		else if (formType == FormTypeEnumeration.UPDATE) {
			form.setName(FORM_EDIT_PREFIX + domainObject.getName() + FORM_EDIT_SUFFIX);
			form.setTitle(FORM_TITLE_EDIT + domainObject.getLabel());
		}
		else if (formType == FormTypeEnumeration.READONLY) {
			form.setName(FORM_READONLY_PREFIX + domainObject.getName() + FORM_READONLY_SUFFIX);
			form.setTitle(FORM_TITLE_READONLY + domainObject.getLabel());
		}

		DTOBean formDTO = shareDTO && domainObject.getSharedDTO() != null ? domainObject.getSharedDTO() : null;

		if (formDTO == null) {
			// Initialize the data transfer object
			formDTO = DtoFactory.eINSTANCE.createDTOBean();
			formDTO.setDomainObject(domainObject);
			formDTO.setPrimitive(false);
			formDTO.setMappable(false);
			formDTO.setShared(shareDTO);
			formDTO.setStandardConversion(true);
			formDTO.setComment("Data transfer object for " + domainObject.getLabel() + " objects");

			if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD)
				formDTO.setName(domainObject.getName() + CREATE_DTO_SUFFIX);
			else if (formType == FormTypeEnumeration.UPDATE)
				formDTO.setName(domainObject.getName() + UPDATE_DTO_SUFFIX);
			else if (formType == FormTypeEnumeration.READONLY)
				formDTO.setName(domainObject.getName() + DTO_SUFFIX);

			if (shareDTO)
				formDTO.setName(domainObject.getName() + DTO_SUFFIX);

			for (final Namespace ns : project.getDTONamespace().getChildNamespaces())
				if (ns.getName().equals(domainObject.getNamespace().getName())) {
					namespaceMap.put(formDTO, ns);
					break;
				}
		}

		form.setDTO(formDTO);
		form.getRoles().addAll(formGroup.getRoles());

		int adminFieldCount = 0;

		// Check if an administration value panel should be added
		if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
			for (final DomainAttribute attr : domainObject.getAllAttributes()) {
				if (attr.isTrackVersion())
					adminFieldCount++;

				if (attr.isPk() && attr.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE)
					adminFieldCount++;

				if (attr.isSetDateOnPersist())
					adminFieldCount++;

				if (attr.isSetDateOnUpdate())
					adminFieldCount++;
			}

		if (adminFieldCount > 1) {
			hasAdminPanel = true;

			adminPanel = ClientFactory.eINSTANCE.createFormPanel();
			adminPanel.setName(PANEL_PREFIX + "Administration");
			adminPanel.setLabel(ADMIN_PANEL_TITLE);
			adminPanel.setColIndex(panelColIndex1++);
			adminPanel.setForm(form);

			form.getFormPanels().add(adminPanel);
		}

		// Add the initial form fields
		for (final DomainAttribute attr : domainObject.getAllAttributes()) {
			if ((formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD) && !attr.isPersistent())
				continue;

			// Skip fields containing password data! The user can change the password in a separate dialog!
			if ((formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
					&& attr.getTag() == AttributeTagEnumeration.USER_PASSWORD)
				continue;

			if (attr.getCollectionType() != CollectionTypeEnumeration.NONE) {
				hasElementCollectionPanel = true;

				final FormPanel collectionEditorPanel = ClientFactory.eINSTANCE.createFormPanel();
				collectionEditorPanel.setColIndex(panelColIndex2++);
				collectionEditorPanel.setRowIndex(2);
				collectionEditorPanel.setName(PANEL_PREFIX + attr.getUpperCaseName());
				collectionEditorPanel.setLabel(attr.getLabel().substring(0, 1).toUpperCase() + attr.getLabel().substring(1));
				collectionEditorPanel.setForm(form);

				form.getFormPanels().add(collectionEditorPanel);

				addFormField(formDTO, collectionEditorPanel, attr, "", !attr.isPersistent(), null);
			}
			else if (adminFieldCount < 2)
				addFormField(formDTO, formPanel, attr, "", !attr.isPersistent(), null);
			else if (attr.isTrackVersion() || attr.isSetDateOnUpdate() || attr.isSetDateOnPersist()
					|| (attr.isPk() && attr.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE))
				addFormField(formDTO, adminPanel, attr, "", true, null);
			else
				addFormField(formDTO, formPanel, attr, "", !attr.isPersistent(), null);
		}

		// Add fields for the first-level associations
		for (final AbstractDomainAssociation assoc : domainObject.getAllAssociations()) {
			// Do not add bidirectional one-to-many associations when creating new records!
			if ((formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD)
					&& assoc instanceof final OneToManyAssociation otm && otm.isBidirectional())
				continue;

			// Do not add many-to-many associations when creating new records if the association is not the owner!
			if ((formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD)
					&& assoc instanceof ManyToManyAssociation && !assoc.isOwner())
				continue;

			if (assoc instanceof ManyToManyAssociation) {
				final String panelName = assoc.getUpperCaseName();
				final String panelLabel = createPanelLabel(panelName);

				if (formType != FormTypeEnumeration.READONLY && assoc.isOwner()) {
					hasSearchableListPanel = true;

					final FormPanel manyToManyFormPanel = ClientFactory.eINSTANCE.createFormPanel();
					manyToManyFormPanel.setName(PANEL_PREFIX + panelName);
					manyToManyFormPanel.setLabel(panelLabel);
					manyToManyFormPanel.setColIndex(panelColIndex2++);
					manyToManyFormPanel.setRowIndex(2);
					manyToManyFormPanel.setForm(form);

					form.getFormPanels().add(manyToManyFormPanel);

					addFormList(formDTO, manyToManyFormPanel, Arrays.asList(assoc));
				}
				else {
					hasTablePanel = true;

					for (final FormPanel panel : project.getAllGridPanelsOfProject())
						if (assoc.equals(panel.getAssociation())) {
							final FormPanel manyToManyPanel = ClientFactory.eINSTANCE.createFormPanel();
							manyToManyPanel.setBasePanel(panel);
							manyToManyPanel.setColIndex(panelColIndex2++);
							manyToManyPanel.setRowIndex(2);
							manyToManyPanel.setName(PANEL_PREFIX + panelName);
							manyToManyPanel.setLabel(panelLabel);
							manyToManyPanel.setForm(form);

							form.getFormPanels().add(manyToManyPanel);
							break;
						}
				}
			}
			else if (assoc instanceof final ManyToOneAssociation manyToOne) {
				if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD && manyToOne.isInsertable())
					addFormList(formDTO, formPanel, Arrays.asList(manyToOne));

				if (formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
					addFormList(formDTO, formPanel, Arrays.asList(manyToOne));
			}
			else if (assoc instanceof OneToOneAssociation) {
				if (!assoc.isOwner())
					continue;

				final DomainObject oneToOneBean = assoc.getTarget();
				final String panelName = assoc.getUpperCaseName();
				final String panelLabel = createPanelLabel(panelName);

				final FormPanel oneToOneFormPanel = ClientFactory.eINSTANCE.createFormPanel();
				oneToOneFormPanel.setName(PANEL_PREFIX + panelName);
				oneToOneFormPanel.setLabel(panelLabel);
				oneToOneFormPanel.setColIndex(panelColIndex1++);
				oneToOneFormPanel.setForm(form);

				form.getFormPanels().add(oneToOneFormPanel);

				// Add simple form fields
				for (final DomainAttribute attr : oneToOneBean.getAllAttributes()) {
					if ((formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD) && !attr.isPersistent())
						continue;

					// Skip fields containing password data! The user can change the password in a separate dialog!
					if ((formType == FormTypeEnumeration.UPDATE || formType == FormTypeEnumeration.READONLY)
							&& attr.getTag() == AttributeTagEnumeration.USER_PASSWORD)
						continue;

					addFormField(formDTO, oneToOneFormPanel, attr, panelName, !attr.isPersistent(), Arrays.asList(assoc));
				}

				// Add list fields
				for (final AbstractDomainAssociation manyToOneOfOneToOne : oneToOneBean.getAllAssociations())
					if (manyToOneOfOneToOne instanceof final ManyToOneAssociation manyToOne) {
						if (formType == FormTypeEnumeration.CREATE || formType == FormTypeEnumeration.ADD && manyToOne.isInsertable())
							addFormList(formDTO, oneToOneFormPanel, Arrays.asList(manyToOneOfOneToOne, assoc));

						if (formType == FormTypeEnumeration.READONLY || formType == FormTypeEnumeration.UPDATE)
							addFormList(formDTO, oneToOneFormPanel, Arrays.asList(manyToOneOfOneToOne, assoc));
					}
			}
			else if (assoc instanceof final OneToManyAssociation otm) {
				final String panelName = otm.getUpperCaseName();
				final String panelLabel = createPanelLabel(panelName);

				if (otm.isBidirectional() || formType == FormTypeEnumeration.READONLY) {
					hasTablePanel = true;

					for (final FormPanel panel : project.getAllGridPanelsOfProject())
						if (otm.equals(panel.getAssociation())) {
							final FormPanel oneToManyPanel = ClientFactory.eINSTANCE.createFormPanel();
							oneToManyPanel.setBasePanel(panel);
							oneToManyPanel.setColIndex(panelColIndex2++);
							oneToManyPanel.setRowIndex(2);
							oneToManyPanel.setName(PANEL_PREFIX + panelName);
							oneToManyPanel.setLabel(panelLabel);
							oneToManyPanel.setForm(form);

							form.getFormPanels().add(oneToManyPanel);
							break;
						}
				}
				else {
					hasSearchableListPanel = true;

					final FormPanel oneToManyFormPanel = ClientFactory.eINSTANCE.createFormPanel();
					oneToManyFormPanel.setName(PANEL_PREFIX + panelName);
					oneToManyFormPanel.setLabel(panelLabel);
					oneToManyFormPanel.setColIndex(panelColIndex2++);
					oneToManyFormPanel.setRowIndex(2);
					oneToManyFormPanel.setForm(form);

					form.getFormPanels().add(oneToManyFormPanel);

					addFormList(formDTO, oneToManyFormPanel, Arrays.asList(otm));
				}
			}
		}

		// Optimize the layout of the main panel!
		FormLayoutOptimizer.optimizeLayout(formPanel);

		if (isOpenedInNewWindow())
			calculateFormSizeProposal(form, formPanel, hasSearchableListPanel, hasTablePanel, hasAdminPanel, hasElementCollectionPanel);
	}

	/**
	 * @param form
	 * @param mainPanel
	 * @param hasSearchableListPanel
	 * @param hasTablePanel
	 * @param hasAdminPanel
	 * @param hasElementCollectionPanel
	 */
	private void calculateFormSizeProposal(Form form, FormPanel mainPanel, boolean hasSearchableListPanel, boolean hasTablePanel,
			boolean hasAdminPanel, boolean hasElementCollectionPanel) {
		int formHeight = DEFAULT_FORM_HEIGHT;
		int formWidth;
		int currentRowIndex = 0;

		// Calculate the height and the width of the form
		for (final FormField f : mainPanel.getFields()) {
			if (!f.isVisible())
				continue;

			if (f.getRowIndex() == currentRowIndex) {
				currentRowIndex = f.getRowIndex();
				continue;
			}

			if (f.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_LABEL
					|| f.getFieldType() == FormFieldTypeEnumeration.MULTI_LINE_TEXT)
				formHeight += MULTI_TEXT_FIELD_HEIGHT;
			else
				formHeight += TEXT_FIELD_HEIGHT;

			currentRowIndex = f.getRowIndex();
		}

		if (hasSearchableListPanel || hasTablePanel || hasElementCollectionPanel) {
			// By default, list or grid panels are placed in the second row! We have to increase the form height accordingly!
			form.setResizable(true);
			formHeight += 300;
		}

		if (hasAdminPanel && formHeight < MIN_FORM_HEIGHT_WITH_ADMIN_PANEL)
			formHeight = MIN_FORM_HEIGHT_WITH_ADMIN_PANEL;

		if (formHeight < MIN_SIMPLE_FORM_HEIGHT)
			formHeight = MIN_SIMPLE_FORM_HEIGHT;

		formWidth = (int) Math.round(formHeight * FORM_WIDTH_FACTOR);

		if (formWidth < MIN_SIMPLE_FORM_WIDTH)
			formWidth = MIN_SIMPLE_FORM_WIDTH;

		// Limit the size of the form
		if (formWidth > MAX_FORM_WIDTH)
			formWidth = MAX_FORM_WIDTH;

		if (formHeight > MAX_FORM_HEIGHT)
			formHeight = MAX_FORM_HEIGHT;

		form.setHeight(formHeight);
		form.setWidth(formWidth);
	}

	/**
	 * Create the default label for a panel based on its name
	 * @param panelName
	 * @return the generated label
	 */
	private String createPanelLabel(final String panelName) {
		final String panelLabel = EclipseIDEService.buildDefaultLabel(panelName);
		return panelLabel.substring(0, 1).toUpperCase() + panelLabel.substring(1);
	}

}
