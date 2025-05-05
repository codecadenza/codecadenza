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
package net.codecadenza.eclipse.ui.panel;

import static net.codecadenza.eclipse.shared.Constants.IMG_ATTRIBUTE;

import java.util.Arrays;
import java.util.List;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.CollectionTypeEnumeration;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.IDGeneratorTypeEnumeration;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.ContentTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethodTypeEnumeration;
import net.codecadenza.eclipse.model.exchange.ExchangeFactory;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingAttribute;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.JavaTypeModifierEnumeration;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.resource.CodeCadenzaResourcePlugin;
import net.codecadenza.eclipse.ui.CodeCadenzaUserInterfacePlugin;
import net.codecadenza.eclipse.ui.dialog.exchange.CreateNewDataExchangeElementDialog;
import net.codecadenza.eclipse.ui.dialog.exchange.EditDataExchangeAttributeDialog;
import net.codecadenza.eclipse.ui.dialog.exchange.EditDataExchangeElementDialog;
import org.eclipse.emf.common.util.BasicEList;
import org.eclipse.emf.common.util.EList;
import org.eclipse.jface.dialogs.Dialog;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.dnd.DND;
import org.eclipse.swt.dnd.DragSource;
import org.eclipse.swt.dnd.DragSourceAdapter;
import org.eclipse.swt.dnd.DragSourceEvent;
import org.eclipse.swt.dnd.DropTarget;
import org.eclipse.swt.dnd.DropTargetAdapter;
import org.eclipse.swt.dnd.DropTargetEvent;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.events.MouseAdapter;
import org.eclipse.swt.events.MouseEvent;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Menu;
import org.eclipse.swt.widgets.MenuItem;
import org.eclipse.swt.widgets.Tree;
import org.eclipse.swt.widgets.TreeItem;
import org.eclipse.ui.ISharedImages;
import org.eclipse.ui.PlatformUI;

/**
 * <p>
 * Panel for maintaining data of a data exchange element
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class EditDataExchangeElementPanel extends Composite {
	private static final String MAPPING_OBJ_KEY = "MAPPING";
	private static final String DEFAULT_EXCEL_ELEMENT_NAME = "Sheet";
	private static final String ITEM_ATTRIBUTES_LABEL = "Attributes";
	private static final String ITEM_ELEMENTS_LABEL = "Elements";
	private static final String TYPE_NAME_SUFFIX = "Type";
	private static final String ROOT_TYPE_NAME_SUFFIX = "RootType";
	private static final String MAPPING_TYPE_SUFFIX = "ExchangeDTO";
	private static final String MAPPING_ROOT_TYPE_SUFFIX = "RootDTO";
	private static final String DEFAULT_JOIN_EXCHANGE_ATTR_NAME = "JOIN_COLUMN";
	private static final String DEFAULT_JOIN_MAPPING_ATTR_NAME = "joinColumnValue";
	private static final String DND_SOURCE_MAPPING = "MAPPING";
	private static final String TREE_ITEM_LABEL_CONTR_QUERY = "Attributes for reference determination";
	private static final String TREE_ITEM_LABEL_CONTR_PERSIST = "Attributes for creation of missing references";
	private static final String DLG_TITLE_EDIT = "Edit data exchange element";
	private static final String DLG_TITLE_NEW = "Create new data exchange element";

	private DataExchangeMethodTypeEnumeration methodType;
	private ContentTypeEnumeration contentType;
	private DomainObject domainObject;
	private final Project project;
	private boolean editMode;
	private String dateFormat;
	private String dateTimeFormat;
	private String numberFormat;
	private EList<AssociationController> associationControllers = new BasicEList<>();
	private boolean singleObject;
	private boolean joined;
	private DataExchangeElement dataExchangeElement;
	private DomainObjectTreePanel domainObjectTreePanel;
	private Tree treeMapping;
	private Menu mnuAttribute;
	private Menu mnuElement;
	private Menu mnuAttributes;
	private Menu mnuElements;
	private Label lblAssocController;
	private Tree treeAssocController;
	private Menu mnuController;
	private Menu mnuControllerAttr;
	private String title = DLG_TITLE_NEW;

	/**
	 * Constructor
	 * @param parent
	 * @param project
	 */
	public EditDataExchangeElementPanel(Composite parent, Project project) {
		super(parent, SWT.NONE);

		this.project = project;

		initializePanel();
	}

	/**
	 * @return the data exchange element
	 */
	public DataExchangeElement getDataExchangeElement() {
		return this.dataExchangeElement;
	}

	/**
	 * @param dataExchangeElement
	 */
	public void setDataExchangeElement(DataExchangeElement dataExchangeElement) {
		this.dataExchangeElement = dataExchangeElement;
	}

	/**
	 * @param methodType
	 */
	public void setMethodType(DataExchangeMethodTypeEnumeration methodType) {
		this.methodType = methodType;
	}

	/**
	 * @param contentType
	 */
	public void setContentType(ContentTypeEnumeration contentType) {
		this.contentType = contentType;
	}

	/**
	 * @param domainObject
	 */
	public void setDomainObject(DomainObject domainObject) {
		this.domainObject = domainObject;
	}

	/**
	 * @param dateFormat
	 */
	public void setDateFormat(String dateFormat) {
		this.dateFormat = dateFormat;
	}

	/**
	 * @param dateTimeFormat
	 */
	public void setDateTimeFormat(String dateTimeFormat) {
		this.dateTimeFormat = dateTimeFormat;
	}

	/**
	 * @param numberFormat
	 */
	public void setNumberFormat(String numberFormat) {
		this.numberFormat = numberFormat;
	}

	/**
	 * @param singleObject
	 */
	public void setSingleObject(boolean singleObject) {
		this.singleObject = singleObject;
	}

	/**
	 * @param joined
	 */
	public void setJoined(boolean joined) {
		this.joined = joined;
	}

	/**
	 * @return a list containing all association controllers
	 */
	public EList<AssociationController> getAssociationControllers() {
		return associationControllers;
	}

	/**
	 * @param associationControllers
	 */
	public void setAssociationControllers(EList<AssociationController> associationControllers) {
		this.associationControllers = associationControllers;
	}

	/**
	 * @param editMode
	 */
	public void setEditMode(boolean editMode) {
		this.editMode = editMode;

		if (editMode)
			this.title = DLG_TITLE_EDIT;
		else
			this.title = DLG_TITLE_NEW;
	}

	/**
	 * @param parentItem
	 * @param element
	 * @param mappingObject
	 * @param addContainerItems
	 */
	private void addDefaultTreeItemsForElement(TreeItem parentItem, DataExchangeElement element,
			ExchangeMappingObject mappingObject, boolean addContainerItems) {
		TreeItem itemSubElements = null;

		if (addContainerItems && (contentType == ContentTypeEnumeration.XML || contentType == ContentTypeEnumeration.JSON)) {
			itemSubElements = new TreeItem(parentItem, SWT.NONE);
			itemSubElements.setText(ITEM_ELEMENTS_LABEL);
			itemSubElements.setData(MAPPING_OBJ_KEY, mappingObject);
			itemSubElements.setData(element);
			itemSubElements.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		}
		else
			itemSubElements = parentItem;

		if (editMode) {
			for (final DataExchangeElement subElement : element.getSubElements()) {
				final var itemElement = new TreeItem(itemSubElements, SWT.NONE);
				itemElement.setText(subElement.getName());
				itemElement.setData(subElement);

				if (subElement.isContainer()) {
					itemElement.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
					itemElement.setData(MAPPING_OBJ_KEY, subElement.getMappingObject());

					addDefaultTreeItemsForElement(itemElement, subElement, subElement.getMappingObject(), true);
				}
				else
					itemElement.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
			}
		}

		if (addContainerItems) {
			final var itemAttributes = new TreeItem(parentItem, SWT.NONE);
			itemAttributes.setText(ITEM_ATTRIBUTES_LABEL);
			itemAttributes.setData(MAPPING_OBJ_KEY, mappingObject);
			itemAttributes.setData(element);
			itemAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));

			element.getAttributes().forEach(attr -> {
				final var itemAttr = new TreeItem(itemAttributes, SWT.NONE);
				itemAttr.setText(attr.getName());
				itemAttr.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
				itemAttr.setData(attr);
			});
		}
	}

	/**
	 * @param mappingObject
	 * @param isRootMappingObject
	 * @return the generated default comment for a mapping object
	 */
	private String createDefaultMappingObjectComment(ExchangeMappingObject mappingObject, boolean isRootMappingObject) {
		final var comment = new StringBuilder();
		final DomainObject domainObjectOfMappingObj = mappingObject.getDomainObject();

		if (isRootMappingObject)
			comment.append("Root mapping");
		else
			comment.append("Mapping");

		comment.append(" object holding " + domainObjectOfMappingObj.getLabel() + " data that is exchanged between ");

		if (contentType == ContentTypeEnumeration.EXCEL97)
			comment.append("Microsoft Excel 97-2003 files");
		else if (contentType == ContentTypeEnumeration.EXCEL2007)
			comment.append("Microsoft Excel 2007 files");
		else
			comment.append("data in " + contentType.getName() + " format");

		comment.append(" and domain object {@link ");
		comment.append(domainObjectOfMappingObj.getNamespace().toString() + "." + domainObjectOfMappingObj.getName());
		comment.append("}");

		return comment.toString();
	}

	/**
	 * Create the initial root element
	 */
	private void createRootElement() {
		TreeItem itemMappingRootElement = null;

		final ExchangeMappingObject rootMappingObject = ExchangeFactory.eINSTANCE.createExchangeMappingObject();
		rootMappingObject.setName(domainObject.getName() + MAPPING_TYPE_SUFFIX);
		rootMappingObject.setDomainObject(domainObject);
		rootMappingObject.setAddNewItems(true);
		rootMappingObject.setUpdateExistingItems(true);
		rootMappingObject.setComment(createDefaultMappingObjectComment(rootMappingObject, true));

		final DataExchangeElement rootElement = ExchangeFactory.eINSTANCE.createDataExchangeElement();
		rootElement.setMaxOccurrences(1);
		rootElement.setMinOccurrences(1);
		rootElement.setTypeName(domainObject.getName() + TYPE_NAME_SUFFIX);
		rootElement.setMappingObject(rootMappingObject);
		rootElement.setContainer(true);

		dataExchangeElement = rootElement;

		if (contentType == ContentTypeEnumeration.XML || contentType == ContentTypeEnumeration.JSON) {
			var elementName = "";

			if (!singleObject)
				elementName = domainObject.getLabelPlural();
			else
				elementName = domainObject.getLabel();

			rootElement.setName(elementName.toLowerCase().replace(" ", "_").replace(".", ""));
		}
		else
			rootElement.setName(DEFAULT_EXCEL_ELEMENT_NAME + 1);

		final var itemRootElement = new TreeItem(treeMapping, SWT.NONE);
		itemRootElement.setText(rootElement.getName());
		itemRootElement.setData(rootElement);
		itemRootElement.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		itemRootElement.setData(MAPPING_OBJ_KEY, rootMappingObject);

		if (contentType == ContentTypeEnumeration.XML) {
			TreeItem parentItem = itemRootElement;
			DataExchangeElement parentElement = rootElement;
			ExchangeMappingObject mappingObject = rootMappingObject;

			if (!singleObject) {
				final String elementName = domainObject.getLabel().toLowerCase().replace(" ", "_").replace(".", "");
				final String listAttributeName = domainObject.getNamePlural().substring(0, 1).toLowerCase()
						+ domainObject.getNamePlural().substring(1);

				rootMappingObject.setName(domainObject.getName() + MAPPING_ROOT_TYPE_SUFFIX);
				rootElement.setTypeName(domainObject.getName() + ROOT_TYPE_NAME_SUFFIX);

				mappingObject = ExchangeFactory.eINSTANCE.createExchangeMappingObject();
				mappingObject.setName(domainObject.getName() + MAPPING_TYPE_SUFFIX);
				mappingObject.setDomainObject(domainObject);
				mappingObject.setComment(createDefaultMappingObjectComment(mappingObject, false));

				// We need a mapping attribute for the root element in order to control generator output!
				final ExchangeMappingAttribute rootMappingAttribute = ExchangeFactory.eINSTANCE.createExchangeMappingAttribute();
				rootMappingAttribute.setModifier(JavaTypeModifierEnumeration.LIST);
				rootMappingAttribute.setName(listAttributeName);
				rootMappingAttribute.setAddNewItems(true);
				rootMappingAttribute.setUpdateExistingItems(true);
				rootMappingAttribute.setDeleteAllItems(false);
				rootMappingAttribute.setExchangeMappingObject(rootMappingObject);

				rootMappingObject.getAttributes().add(rootMappingAttribute);

				final DataExchangeElement mappingRootElement = ExchangeFactory.eINSTANCE.createDataExchangeElement();
				mappingRootElement.setName(elementName);
				mappingRootElement.setMinOccurrences(0);
				mappingRootElement.setParentElement(rootElement);
				mappingRootElement.setTypeName(domainObject.getName() + TYPE_NAME_SUFFIX);
				mappingRootElement.setMappingObject(mappingObject);
				mappingRootElement.setContainer(true);
				mappingRootElement.setMappingAttribute(rootMappingAttribute);

				itemMappingRootElement = new TreeItem(itemRootElement, SWT.NONE);
				itemMappingRootElement.setText(mappingRootElement.getName());
				itemMappingRootElement.setData(mappingRootElement);
				itemMappingRootElement.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
				itemMappingRootElement.setData(MAPPING_OBJ_KEY, mappingObject);

				parentItem = itemMappingRootElement;
				parentElement = mappingRootElement;
			}

			addDefaultTreeItemsForElement(parentItem, parentElement, mappingObject, true);
		}
		else if (contentType == ContentTypeEnumeration.JSON)
			addDefaultTreeItemsForElement(itemRootElement, rootElement, rootMappingObject, true);
		else {
			final var itemAttributes = new TreeItem(itemRootElement, SWT.NONE);
			itemAttributes.setText(ITEM_ATTRIBUTES_LABEL);
			itemAttributes.setData(rootElement);
			itemAttributes.setData(MAPPING_OBJ_KEY, rootMappingObject);
			itemAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		}

		itemRootElement.setExpanded(true);

		if (itemMappingRootElement != null)
			itemMappingRootElement.setExpanded(true);
	}

	/**
	 * @param mappingAttribute
	 * @return true if the respective exchange attribute or the exchange element can be optional
	 */
	private boolean isOptional(ExchangeMappingAttribute mappingAttribute) {
		final DomainAttribute attribute = mappingAttribute.getDomainAttribute();

		if (attribute.getDomainAttributeValidator().isNullable())
			return true;

		if (mappingAttribute.getAssociation() == null && methodType == DataExchangeMethodTypeEnumeration.IMPORT) {
			// An attribute or an element representing an auto-generated primary key can be optional!
			if (attribute.isPk() && attribute.getDomainObject().getIDGenerator().getGeneratorType() != IDGeneratorTypeEnumeration.NONE)
				return true;

			// Fields that are maintained by the persistence provider can be optional also!
			if (attribute.isSetDateOnPersist() || attribute.isTrackVersion())
				return true;
		}

		return false;
	}

	/**
	 * Add new data exchange attribute
	 * @param targetTreeItem
	 * @param sourceTreeItem
	 */
	private void addNewDataExchangeAttribute(TreeItem targetTreeItem, TreeItem sourceTreeItem) {
		final var attribute = (DomainAttribute) sourceTreeItem.getData();
		final var parentElement = (DataExchangeElement) targetTreeItem.getData();
		final var mappingObject = (ExchangeMappingObject) targetTreeItem.getData(MAPPING_OBJ_KEY);
		final List<AbstractDomainAssociation> assocList = domainObjectTreePanel.getAssociationListOfSelectedTreeItem();

		// Check if the mapping attribute is allowed to be added to this element
		if (!mappingObject.getDomainObject().getAllValidDTOAttributes().contains(attribute))
			return;

		final ExchangeMappingAttribute mappingAttribute = ExchangeFactory.eINSTANCE.createExchangeMappingAttribute();
		mappingAttribute.setName(attribute.getName());
		mappingAttribute.setDomainAttribute(attribute);
		mappingAttribute.setInsertable(attribute.isInsertable());
		mappingAttribute.setUpdatable(attribute.isUpdatable());
		mappingAttribute.setExchangeMappingObject(mappingObject);
		mappingAttribute.setModifier(JavaTypeModifierEnumeration.NONE);

		if (!assocList.isEmpty()) {
			final AbstractDomainAssociation firstAssoc = assocList.get(0);
			mappingAttribute.setName(firstAssoc.getName() + attribute.getUpperCaseName());

			mappingAttribute.setAssociation(firstAssoc);

			// Do not add the full association list! Just add all elements after the first one!
			if (assocList.size() > 1)
				for (int i = 1; i < assocList.size(); i++)
					mappingAttribute.getAssociationList().add(assocList.get(i));
		}

		mappingObject.getAttributes().add(mappingAttribute);

		final DataExchangeAttribute dataExchangeAttribute = ExchangeFactory.eINSTANCE.createDataExchangeAttribute();
		dataExchangeAttribute.setName(mappingAttribute.getName());
		dataExchangeAttribute.setOptional(isOptional(mappingAttribute));
		dataExchangeAttribute.setVisible(true);
		dataExchangeAttribute.setMappingAttribute(mappingAttribute);
		dataExchangeAttribute.setElement(parentElement);

		if (contentType == ContentTypeEnumeration.EXCEL97 || contentType == ContentTypeEnumeration.EXCEL2007
				|| contentType == ContentTypeEnumeration.CSV) {
			var attributeName = "";

			if (mappingAttribute.getAssociation() != null) {
				final String assocName = mappingAttribute.getAssociation().getName();
				attributeName = assocName.substring(0, 1).toUpperCase() + assocName.substring(1);
				attributeName += " ";
				attributeName += attribute.getLabel();
			}
			else
				attributeName = attribute.getLabel().substring(0, 1).toUpperCase() + attribute.getLabel().substring(1);

			dataExchangeAttribute.setName(attributeName);

			final JavaType type = attribute.getJavaType();

			if (type.isTemporalType()) {
				if (type.isLocalDate() || attribute.getTemporalType() == TemporalTypeEnumeration.DATE)
					dataExchangeAttribute.setFormat(dateFormat);
				else
					dataExchangeAttribute.setFormat(dateTimeFormat);
			}

			if (type.isDecimalNumber())
				dataExchangeAttribute.setFormat(numberFormat);
		}

		parentElement.getAttributes().add(dataExchangeAttribute);

		final var itemMapping = new TreeItem(targetTreeItem, SWT.NONE);
		itemMapping.setText(dataExchangeAttribute.getName());
		itemMapping.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
		itemMapping.setData(dataExchangeAttribute);
	}

	/**
	 * Add a new data exchange element
	 * @param targetTreeItem
	 * @param sourceTreeItem
	 */
	private void addNewDataExchangeElement(TreeItem targetTreeItem, TreeItem sourceTreeItem) {
		final var parentElement = (DataExchangeElement) targetTreeItem.getData();
		final var attribute = (DomainAttribute) sourceTreeItem.getData();
		final var mappingObject = (ExchangeMappingObject) targetTreeItem.getData(MAPPING_OBJ_KEY);
		final List<AbstractDomainAssociation> assocList = domainObjectTreePanel.getAssociationListOfSelectedTreeItem();

		// Check if the mapping attribute is allowed to be added to this element
		if (!mappingObject.getDomainObject().getAllValidDTOAttributes().contains(attribute))
			return;

		final ExchangeMappingAttribute mappingAttribute = ExchangeFactory.eINSTANCE.createExchangeMappingAttribute();
		mappingAttribute.setName(attribute.getName());
		mappingAttribute.setDomainAttribute(attribute);
		mappingAttribute.setInsertable(attribute.isInsertable());
		mappingAttribute.setUpdatable(attribute.isUpdatable());
		mappingAttribute.setExchangeMappingObject(mappingObject);
		mappingAttribute.setModifier(JavaTypeModifierEnumeration.NONE);

		if (!assocList.isEmpty()) {
			final AbstractDomainAssociation firstAssoc = assocList.get(0);

			mappingAttribute.setAssociation(firstAssoc);
			mappingAttribute.setName(firstAssoc.getName() + attribute.getUpperCaseName());

			// Do not add the full association list! Just add all elements after the first one!
			if (assocList.size() > 1)
				for (int i = 1; i < assocList.size(); i++)
					mappingAttribute.getAssociationList().add(assocList.get(i));
		}

		mappingObject.getAttributes().add(mappingAttribute);

		final DataExchangeElement element = ExchangeFactory.eINSTANCE.createDataExchangeElement();
		element.setName(mappingAttribute.getName());
		element.setMappingAttribute(mappingAttribute);
		element.setParentElement(parentElement);
		element.setMappingObject(mappingObject);
		element.setContainer(false);
		element.setMinOccurrences(1);

		if (attribute.getCollectionType() == CollectionTypeEnumeration.NONE)
			element.setMaxOccurrences(1);

		if (isOptional(mappingAttribute) || attribute.getCollectionType() != CollectionTypeEnumeration.NONE)
			element.setMinOccurrences(0);

		parentElement.getSubElements().add(element);

		final var itemMapping = new TreeItem(targetTreeItem, SWT.NONE);
		itemMapping.setText(element.getName());
		itemMapping.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
		itemMapping.setData(element);
		itemMapping.setExpanded(true);
	}

	/**
	 * Add a new element
	 * @param targetTreeItem
	 * @param assoc
	 */
	private void addNewDataExchangeElement(TreeItem targetTreeItem, AbstractDomainAssociation assoc) {
		final var parentElement = (DataExchangeElement) targetTreeItem.getData();
		final var parentMappingObject = (ExchangeMappingObject) targetTreeItem.getData(MAPPING_OBJ_KEY);

		// Check if the association element is allowed to be added to this element!
		final boolean assocFound = parentMappingObject.getDomainObject().getAllAssociations().stream().anyMatch(a -> a.equals(assoc));

		if (!assocFound)
			return;

		final ExchangeMappingObject mappingObject = ExchangeFactory.eINSTANCE.createExchangeMappingObject();
		mappingObject.setName(assoc.getTarget().getName() + MAPPING_TYPE_SUFFIX);
		mappingObject.setDomainObject(assoc.getTarget());
		mappingObject.setComment(createDefaultMappingObjectComment(mappingObject, false));

		// The parent mapping objects needs a new attribute!
		final ExchangeMappingAttribute mappingAttribute = ExchangeFactory.eINSTANCE.createExchangeMappingAttribute();
		mappingAttribute.setName(assoc.getName());
		mappingAttribute.setAssociation(assoc);
		mappingAttribute.setModifier(JavaTypeModifierEnumeration.NONE);
		mappingAttribute.setExchangeMappingObject(parentMappingObject);

		// In this case the association list doesn't have to be initialized as the respective mapping field always has to be accessed
		// directly!
		parentMappingObject.getAttributes().add(mappingAttribute);

		final DataExchangeElement element = ExchangeFactory.eINSTANCE.createDataExchangeElement();
		element.setName(mappingAttribute.getName());
		element.setMappingAttribute(mappingAttribute);
		element.setTypeName(assoc.getUpperCaseName() + TYPE_NAME_SUFFIX);
		element.setMappingObject(mappingObject);
		element.setContainer(true);

		if (assoc instanceof final ManyToOneAssociation mto) {
			element.setMaxOccurrences(1);
			element.setMinOccurrences(1);

			if (mto.isOptional())
				element.setMinOccurrences(0);

			mappingAttribute.setInsertable(mto.isInsertable());
			mappingAttribute.setUpdatable(mto.isUpdatable());
		}
		else if (assoc instanceof final OneToOneAssociation oto) {
			element.setMaxOccurrences(1);
			element.setMinOccurrences(1);

			if (oto.isOptional())
				element.setMinOccurrences(0);

			mappingAttribute.setInsertable(true);
			mappingAttribute.setUpdatable(true);
		}
		else if (assoc instanceof ManyToManyAssociation || assoc instanceof OneToManyAssociation) {
			mappingAttribute.setModifier(JavaTypeModifierEnumeration.LIST);

			if (assoc instanceof final OneToManyAssociation otm && otm.isBidirectional()) {
				mappingAttribute.setAddNewItems(true);
				mappingAttribute.setUpdateExistingItems(true);
			}

			if (contentType == ContentTypeEnumeration.XML)
				element.setName(assoc.getTarget().getLowerCaseName());
			else
				element.setName(assoc.getName());

			element.setMinOccurrences(0);
			element.setTypeName(assoc.getTarget().getUpperCaseName() + TYPE_NAME_SUFFIX);
		}

		if (contentType == ContentTypeEnumeration.EXCEL97 || contentType == ContentTypeEnumeration.EXCEL2007)
			element.setName(DEFAULT_EXCEL_ELEMENT_NAME + Integer.toString(parentElement.getSubElements().size() + 2));

		element.setParentElement(parentElement);

		// Add a new item to the tree
		final var itemMapping = new TreeItem(targetTreeItem, SWT.NONE);

		itemMapping.setText(element.getName());
		itemMapping.setData(element);
		itemMapping.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		itemMapping.setData(MAPPING_OBJ_KEY, mappingObject);

		if ((contentType == ContentTypeEnumeration.EXCEL97 || contentType == ContentTypeEnumeration.EXCEL2007) && !singleObject
				&& methodType == DataExchangeMethodTypeEnumeration.IMPORT) {

			// In case of multiple Microsoft Excel sheets it is necessary to create a join attribute in order to properly link objects
			// residing in different sheets!
			final ExchangeMappingAttribute joinAttribute = ExchangeFactory.eINSTANCE.createExchangeMappingAttribute();
			joinAttribute.setName(DEFAULT_JOIN_MAPPING_ATTR_NAME);
			joinAttribute.setJoinAttribute(true);
			joinAttribute.setExchangeMappingObject(mappingObject);
			joinAttribute.setModifier(JavaTypeModifierEnumeration.NONE);
			joinAttribute.setMappingType(assoc.getDomainObject().getPKAttribute().getJavaType());

			mappingObject.getAttributes().add(joinAttribute);

			final DataExchangeAttribute dataExchangeJoinAttribute = ExchangeFactory.eINSTANCE.createDataExchangeAttribute();
			dataExchangeJoinAttribute.setName(DEFAULT_JOIN_EXCHANGE_ATTR_NAME);
			dataExchangeJoinAttribute.setOptional(false);
			dataExchangeJoinAttribute.setVisible(true);
			dataExchangeJoinAttribute.setMappingAttribute(joinAttribute);
			dataExchangeJoinAttribute.setElement(element);
		}

		addDefaultTreeItemsForElement(itemMapping, element, mappingObject, true);

		targetTreeItem.setExpanded(true);
		itemMapping.setExpanded(true);
	}

	/**
	 * Refresh the domain object and the mapping tree views
	 */
	public void refresh() {
		// Remove all items from both trees
		treeMapping.removeAll();
		treeAssocController.removeAll();

		domainObjectTreePanel.setEnabled(true);
		treeMapping.setEnabled(true);
		treeAssocController.setEnabled(true);
		lblAssocController.setEnabled(true);

		if (methodType == DataExchangeMethodTypeEnumeration.EXPORT) {
			lblAssocController.setEnabled(false);
			treeAssocController.setEnabled(false);
		}

		if (!joined) {
			domainObjectTreePanel.init(domainObject);

			if (editMode) {
				final DataExchangeElement rootElement = dataExchangeElement;
				TreeItem itemMappingRootElement = null;
				final var itemRootElement = new TreeItem(treeMapping, SWT.NONE);

				itemRootElement.setText(rootElement.getName());
				itemRootElement.setData(rootElement);
				itemRootElement.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
				itemRootElement.setData(MAPPING_OBJ_KEY, rootElement.getMappingObject());

				if (contentType == ContentTypeEnumeration.XML) {
					TreeItem parentItem = itemRootElement;
					DataExchangeElement parentElement = rootElement;
					ExchangeMappingObject mappingObject = rootElement.getMappingObject();

					if (!singleObject) {
						for (final DataExchangeElement element : rootElement.getSubElements()) {
							itemMappingRootElement = new TreeItem(itemRootElement, SWT.NONE);
							itemMappingRootElement.setText(element.getName());
							itemMappingRootElement.setData(element);
							itemMappingRootElement.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
							itemMappingRootElement.setData(MAPPING_OBJ_KEY, element.getMappingObject());

							parentItem = itemMappingRootElement;
							parentElement = element;
							mappingObject = element.getMappingObject();
						}
					}

					addDefaultTreeItemsForElement(parentItem, parentElement, mappingObject, true);
				}
				else
					addDefaultTreeItemsForElement(itemRootElement, rootElement, rootElement.getMappingObject(), true);

				itemRootElement.setExpanded(true);

				if (itemMappingRootElement != null)
					itemMappingRootElement.setExpanded(true);

				// Add association controllers
				associationControllers.forEach(controller -> {
					final var itemController = new TreeItem(treeAssocController, SWT.NONE);
					itemController.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
					itemController
							.setText(controller.getAssociation().getDomainObject().getName() + " -> " + controller.getAssociation().getName());
					itemController.setData(controller);

					final var itemQueryAttributes = new TreeItem(itemController, SWT.NONE);
					itemQueryAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
					itemQueryAttributes.setText(TREE_ITEM_LABEL_CONTR_QUERY);
					itemQueryAttributes.setData(controller);

					controller.getQueryAttributes().forEach(attr -> {
						final var itemAttr = new TreeItem(itemQueryAttributes, SWT.NONE);
						itemAttr.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
						itemAttr.setText(attr.getName());
						itemAttr.setData(attr);
					});

					final var itemPersistAttributes = new TreeItem(itemController, SWT.NONE);
					itemPersistAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
					itemPersistAttributes.setText(TREE_ITEM_LABEL_CONTR_PERSIST);
					itemPersistAttributes.setData(controller);

					controller.getPersistAttributes().forEach(attr -> {
						final var itemAttr = new TreeItem(itemPersistAttributes, SWT.NONE);
						itemAttr.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
						itemAttr.setText(attr.getName());
						itemAttr.setData(attr);
					});
				});
			}
			else
				createRootElement();
		}
		else {
			domainObjectTreePanel.setEnabled(false);
			treeMapping.setEnabled(false);
		}
	}

	/**
	 * Initialize the menu
	 */
	private void initializeMenu() {
		mnuAttribute = new Menu(treeMapping);

		final var mniEditAttribute = new MenuItem(mnuAttribute, SWT.NONE);
		mniEditAttribute.setText("Edit attribute");

		mniEditAttribute.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final var dataExchangeAttribute = (DataExchangeAttribute) selItem.getData();
				final var dlg = new EditDataExchangeAttributeDialog(getShell(), dataExchangeAttribute, methodType, contentType);

				if (dlg.open() == Dialog.OK)
					selItem.setText(dataExchangeAttribute.getName());
			}
		});

		new MenuItem(mnuAttribute, SWT.SEPARATOR);

		final var mniDeleteAttribute = new MenuItem(mnuAttribute, SWT.NONE);
		mniDeleteAttribute.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
		mniDeleteAttribute.setText("Delete (attribute only)");

		mniDeleteAttribute.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final boolean doIt = MessageDialog.openConfirm(getShell(), title, "Do you really want to delete the selected attribute?");

				if (!doIt)
					return;

				final var attribute = (DataExchangeAttribute) selItem.getData();
				final DataExchangeElement element = attribute.getElement();

				element.getAttributes().remove(attribute);

				if (editMode)
					project.eResource().getContents().remove(attribute);

				selItem.dispose();
			}
		});

		final var mniDeleteAttributeWithMapping = new MenuItem(mnuAttribute, SWT.NONE);
		mniDeleteAttributeWithMapping.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
		mniDeleteAttributeWithMapping.setText("Delete (incl. mapping)");

		mniDeleteAttributeWithMapping.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final boolean doIt = MessageDialog.openConfirm(getShell(), title, "Do you really want to delete the selected attribute?");

				if (!doIt)
					return;

				final var attribute = (DataExchangeAttribute) selItem.getData();
				final DataExchangeElement element = attribute.getElement();
				ExchangeMappingObject mappingObject = null;

				if (attribute.getMappingAttribute() != null) {
					// Remove the mapping attribute from the association controller tree view
					removeAttributeFromAssocTree(attribute.getMappingAttribute());

					mappingObject = attribute.getMappingAttribute().getExchangeMappingObject();
					mappingObject.getAttributes().remove(attribute.getMappingAttribute());
				}

				element.getAttributes().remove(attribute);

				if (editMode) {
					if (attribute.getMappingAttribute() != null)
						project.eResource().getContents().remove(attribute.getMappingAttribute());

					project.eResource().getContents().remove(attribute);
				}

				selItem.dispose();
			}
		});

		mnuElement = new Menu(treeMapping);

		final var mniEditElement = new MenuItem(mnuElement, SWT.NONE);
		mniEditElement.setText("Edit element");

		mniEditElement.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final boolean rootElement = selItem.getParentItem() == null;
				boolean processSingleObject = false;

				if (rootElement && singleObject)
					processSingleObject = true;

				final var element = (DataExchangeElement) selItem.getData();
				final var dlg = new EditDataExchangeElementDialog(getShell(), element, rootElement, contentType, methodType,
						processSingleObject);

				if (dlg.open() == Dialog.OK)
					selItem.setText(element.getName());
			}
		});

		new MenuItem(mnuElement, SWT.SEPARATOR);

		final var mniDeleteElement = new MenuItem(mnuElement, SWT.NONE);
		mniDeleteElement.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
		mniDeleteElement.setText("Delete (element only)");

		mniDeleteElement.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final var element = (DataExchangeElement) selItem.getData();
				final DataExchangeElement parentElement = element.getParentElement();

				if (parentElement == null) {
					MessageDialog.openInformation(getShell(), title, "A root element cannot be deleted!");
					return;
				}

				final boolean doIt = MessageDialog.openConfirm(getShell(), title, "Do you really want to delete the selected element?");

				if (!doIt)
					return;

				if (editMode) {
					element.getAllElements().forEach(subElement -> project.eResource().getContents().remove(subElement));

					parentElement.getSubElements().remove(element);
					project.eResource().getContents().remove(element);
				}
				else
					parentElement.getSubElements().remove(element);

				selItem.dispose();
			}
		});

		final var mniDeleteElementWithMapping = new MenuItem(mnuElement, SWT.NONE);
		mniDeleteElementWithMapping.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_TOOL_DELETE));
		mniDeleteElementWithMapping.setText("Delete (incl. mapping)");

		mniDeleteElementWithMapping.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final var element = (DataExchangeElement) selItem.getData();
				final DataExchangeElement parentElement = element.getParentElement();

				if (parentElement == null) {
					MessageDialog.openInformation(getShell(), title, "A root element cannot be deleted!");
					return;
				}

				final boolean doIt = MessageDialog.openConfirm(getShell(), title, "Do you really want to delete the selected element?");

				if (!doIt)
					return;

				if (editMode) {
					if (element.getMappingAttribute() != null) {
						// Remove the mapping attribute from the association controller tree view
						removeAttributeFromAssocTree(element.getMappingAttribute());

						final ExchangeMappingObject mappingObject = element.getMappingAttribute().getExchangeMappingObject();
						mappingObject.getAttributes().remove(element.getMappingAttribute());

						project.eResource().getContents().remove(element.getMappingAttribute());
					}

					for (final DataExchangeElement subElement : element.getAllElements()) {
						if (!subElement.isContainer())
							continue;

						if (subElement.getMappingObject() != null)
							project.eResource().getContents().remove(subElement.getMappingObject());

						project.eResource().getContents().remove(subElement);
					}

					if (element.isContainer() && element.getMappingObject() != null)
						project.eResource().getContents().remove(element.getMappingObject());

					parentElement.getSubElements().remove(element);
					project.eResource().getContents().remove(element);
				}
				else {
					parentElement.getSubElements().remove(element);

					if (element.getMappingAttribute() != null) {
						final ExchangeMappingObject mappingObject = element.getMappingAttribute().getExchangeMappingObject();
						mappingObject.getAttributes().remove(element.getMappingAttribute());
					}
				}

				selItem.dispose();
			}
		});

		mnuElements = new Menu(treeMapping);

		final var mniAddUnmappedElement = new MenuItem(mnuElements, SWT.NONE);
		mniAddUnmappedElement.setText("Add unmapped element");

		mniAddUnmappedElement.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final var parentElement = (DataExchangeElement) selItem.getData();
				final var dlg = new CreateNewDataExchangeElementDialog(getShell(), parentElement, project,
						CreateNewDataExchangeElementDialog.Mode.UNMAPPED);

				if (dlg.open() == Dialog.OK) {
					final DataExchangeElement element = dlg.getDataExchangeElement();

					final var itemAttribute = new TreeItem(selItem, SWT.NONE);
					itemAttribute.setText(element.getName());
					itemAttribute.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
					itemAttribute.setData(element);
				}
			}
		});

		final var mniAddMappedElement = new MenuItem(mnuElements, SWT.NONE);
		mniAddMappedElement.setText("Add mapped element");

		mniAddMappedElement.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final var parentElement = (DataExchangeElement) selItem.getData();
				final var dlg = new CreateNewDataExchangeElementDialog(getShell(), parentElement, project,
						CreateNewDataExchangeElementDialog.Mode.MAPPED);

				if (dlg.open() == Dialog.OK) {
					final DataExchangeElement element = dlg.getDataExchangeElement();

					final var itemAttribute = new TreeItem(selItem, SWT.NONE);
					itemAttribute.setText(element.getName());
					itemAttribute.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
					itemAttribute.setData(element);
				}
			}
		});

		mnuAttributes = new Menu(treeMapping);

		final var mniAddAttribute = new MenuItem(mnuAttributes, SWT.NONE);
		mniAddAttribute.setText("Add attribute");

		mniAddAttribute.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final var parentElement = (DataExchangeElement) selItem.getData();
				final var dlg = new EditDataExchangeAttributeDialog(getShell(), parentElement, project, methodType, contentType);

				if (dlg.open() == Dialog.OK) {
					final DataExchangeAttribute dataExchangeAttribute = dlg.getDataExchangeAttribute();

					final var itemAttribute = new TreeItem(selItem, SWT.NONE);
					itemAttribute.setText(dataExchangeAttribute.getName());
					itemAttribute.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
					itemAttribute.setData(dataExchangeAttribute);
				}
			}
		});

		mnuController = new Menu(treeAssocController);

		final var mnuDeleteController = new MenuItem(mnuController, SWT.NONE);
		mnuDeleteController.setText("Delete");

		mnuDeleteController.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedControllerItem();

				if (selItem == null || selItem.getData() == null)
					return;

				final var controller = (AssociationController) selItem.getData();

				final boolean doIt = MessageDialog.openConfirm(getShell(), title,
						"Do you really want to delete the selected association controller?");

				if (!doIt)
					return;

				associationControllers.remove(controller);

				selItem.dispose();
			}
		});

		mnuControllerAttr = new Menu(treeAssocController);

		final var mnuDeleteControllerAttr = new MenuItem(mnuControllerAttr, SWT.NONE);
		mnuDeleteControllerAttr.setText("Delete");

		mnuDeleteControllerAttr.addSelectionListener(new SelectionAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.SelectionAdapter#widgetSelected(org.eclipse.swt.events.SelectionEvent)
			 */
			@Override
			public void widgetSelected(SelectionEvent e) {
				final TreeItem selItem = getSelectedControllerItem();
				boolean removeQueryAttr = false;

				if (selItem == null || selItem.getData() == null)
					return;

				if (!(selItem.getData() instanceof final ExchangeMappingAttribute attr))
					return;

				if (selItem.getParentItem().getText().equals(TREE_ITEM_LABEL_CONTR_QUERY))
					removeQueryAttr = true;

				final var controller = (AssociationController) selItem.getParentItem().getData();

				final boolean doIt = MessageDialog.openConfirm(getShell(), title, "Do you really want to delete the selected attribute?");

				if (!doIt)
					return;

				if (removeQueryAttr)
					controller.getQueryAttributes().remove(attr);
				else
					controller.getPersistAttributes().remove(attr);

				selItem.dispose();
			}
		});
	}

	/**
	 * @param parentItem
	 * @param mappingAttr
	 * @param controller
	 */
	private void addMappingAttributeToController(TreeItem parentItem, ExchangeMappingAttribute mappingAttr,
			AssociationController controller) {
		boolean addToQueryAttributes = false;
		boolean valid = false;

		if (parentItem.getText().equals(TREE_ITEM_LABEL_CONTR_QUERY))
			addToQueryAttributes = true;

		// Don't add a mapping attribute twice!
		if (addToQueryAttributes) {
			if (controller.getQueryAttributes().contains(mappingAttr))
				return;
		}
		else if (controller.getPersistAttributes().contains(mappingAttr))
			return;

		// The attribute must be mapped to a domain attribute!
		if (mappingAttr.getDomainAttribute() == null)
			return;

		if (controller.getAssociation() instanceof ManyToOneAssociation) {
			if (mappingAttr.getAssociation() != null) {
				if (!controller.getAssociation().equals(mappingAttr.getAssociation())) {
					final EList<AbstractDomainAssociation> assocList = mappingAttr.getAssociationList();

					// We don't support deeply cascaded associations!
					if (assocList != null && !assocList.isEmpty() && assocList.get(0).equals(controller.getAssociation())
							&& mappingAttr.getAssociation() instanceof ManyToOneAssociation)
						valid = true;
				}
				else
					valid = true;
			}
		}
		else {
			final DomainObject targetDomainObject = controller.getAssociation().getTarget();

			if (mappingAttr.getAssociation() == null) {
				if (mappingAttr.getExchangeMappingObject().getDomainObject().equals(targetDomainObject)
						&& targetDomainObject.getAllAttributes().contains(mappingAttr.getDomainAttribute()))
					valid = true;
			}
			else if (targetDomainObject.getAllAssociations().contains(mappingAttr.getAssociation())
					&& mappingAttr.getAssociation() instanceof ManyToOneAssociation)
				valid = true;
		}

		if (!valid)
			return;

		// Add a mapping attribute to the controller
		final var itemAttr = new TreeItem(parentItem, SWT.NONE);
		itemAttr.setImage(CodeCadenzaResourcePlugin.getImage(IMG_ATTRIBUTE));
		itemAttr.setText(mappingAttr.getName());
		itemAttr.setData(mappingAttr);

		if (addToQueryAttributes)
			controller.getQueryAttributes().add(mappingAttr);
		else
			controller.getPersistAttributes().add(mappingAttr);
	}

	/**
	 * Create an association controller tree item
	 * @param assoc
	 * @return the created tree item
	 */
	private TreeItem addAssociationController(AbstractDomainAssociation assoc) {
		if (assoc instanceof OneToOneAssociation || assoc instanceof final OneToManyAssociation otm && otm.isBidirectional())
			return null;

		// Check if a controller already exists
		for (final AssociationController cont : associationControllers)
			if (cont.getAssociation().equals(assoc))
				return null;

		// Create a new controller
		final var itemController = new TreeItem(treeAssocController, SWT.NONE);
		itemController.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		itemController.setText(assoc.getDomainObject().getName() + " -> " + assoc.getName());

		final AssociationController controller = ExchangeFactory.eINSTANCE.createAssociationController();
		controller.setAssociation(assoc);

		itemController.setData(controller);

		associationControllers.add(controller);

		final var itemQueryAttributes = new TreeItem(itemController, SWT.NONE);
		itemQueryAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		itemQueryAttributes.setText(TREE_ITEM_LABEL_CONTR_QUERY);
		itemQueryAttributes.setData(controller);

		final var itemPersistAttributes = new TreeItem(itemController, SWT.NONE);
		itemPersistAttributes.setImage(PlatformUI.getWorkbench().getSharedImages().getImage(ISharedImages.IMG_OBJ_FOLDER));
		itemPersistAttributes.setText(TREE_ITEM_LABEL_CONTR_PERSIST);
		itemPersistAttributes.setData(controller);

		itemController.setExpanded(true);

		return itemQueryAttributes;
	}

	/**
	 * Remove the mapping attribute from the association controller tree view
	 * @param parentItem
	 * @param attr
	 */
	private void removeAttributeFromAssocTree(TreeItem parentItem, ExchangeMappingAttribute attr) {
		final var controller = (AssociationController) parentItem.getData();

		// The third level contains all mapping attributes defined for a given controller
		for (final TreeItem folterItem : parentItem.getItems())
			for (final TreeItem assocItem : folterItem.getItems())
				if (assocItem.getData().equals(attr)) {
					controller.getQueryAttributes().remove(attr);
					assocItem.dispose();
					break;
				}
	}

	/**
	 * Remove the mapping attribute from the association controller tree view
	 * @param attr
	 */
	private void removeAttributeFromAssocTree(ExchangeMappingAttribute attr) {
		// The first level contains all controller objects
		for (final TreeItem controllerItem : treeAssocController.getItems())
			removeAttributeFromAssocTree(controllerItem, attr);
	}

	/**
	 * Initialize this panel
	 */
	protected void initializePanel() {
		this.setLayout(new FillLayout());

		final var sashForm = new SashForm(this, SWT.NONE);

		final var panDomainObjectTree = new Composite(sashForm, SWT.NONE);
		panDomainObjectTree.setLayout(new GridLayout());

		final var lblDomainObjectTree = new Label(panDomainObjectTree, SWT.NONE);
		lblDomainObjectTree.setText("Drag and drop items to be exchanged:");

		domainObjectTreePanel = new DomainObjectTreePanel(panDomainObjectTree, DomainObjectTreePanel.Mode.EXCHANGE) {
			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.util.DomainObjectTreePanel#addDomainAttribute(net.codecadenza.eclipse.model.
			 * domain.DomainAttribute)
			 */
			@Override
			protected boolean addDomainAttribute(DomainAttribute attr) {
				// Domain attributes that are mapped to an element collection are only supported for XML and JSON!
				if (attr.getCollectionType() != CollectionTypeEnumeration.NONE)
					return contentType == ContentTypeEnumeration.XML || contentType == ContentTypeEnumeration.JSON;

				return !attr.isLob();
			}

			/*
			 * (non-Javadoc)
			 * @see net.codecadenza.eclipse.ui.dialog.util.DomainObjectTreePanel#addDomainAssociation(net.codecadenza.eclipse.model.
			 * domain.AbstractDomainAssociation)
			 */
			@Override
			protected boolean addDomainAssociation(AbstractDomainAssociation assoc) {
				// We don't support recursive structures based on one-to-many and one-to-one associations!
				if ((assoc instanceof OneToManyAssociation || assoc instanceof OneToOneAssociation)
						&& assoc.getDomainObject().equals(assoc.getTarget()))
					return false;

				// Handling of one-to-many and many-to-many associations is not supported for CSV!
				return !(contentType == ContentTypeEnumeration.CSV
						&& (assoc instanceof OneToManyAssociation || assoc instanceof ManyToManyAssociation));
			}
		};

		domainObjectTreePanel.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		final var panMappingTree = new Composite(sashForm, SWT.NONE);
		panMappingTree.setLayout(new FillLayout());

		final var sashFormMapping = new SashForm(panMappingTree, SWT.VERTICAL);

		final var panMappingElements = new Composite(sashFormMapping, SWT.NONE);
		panMappingElements.setLayout(new GridLayout());

		final var lblMappingTree = new Label(panMappingElements, SWT.NONE);
		lblMappingTree.setText("Mapping (file) structure:");

		treeMapping = new Tree(panMappingElements, SWT.BORDER);
		treeMapping.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		treeMapping.addMenuDetectListener(e -> {
			treeMapping.setMenu(null);

			final TreeItem selItem = getSelectedMappingItem();

			if (selItem == null || selItem.getData() == null)
				return;

			if (selItem.getText().equals(ITEM_ELEMENTS_LABEL)) {
				if (contentType == ContentTypeEnumeration.XML)
					treeMapping.setMenu(mnuElements);

				return;
			}

			if (selItem.getText().equals(ITEM_ATTRIBUTES_LABEL)) {
				treeMapping.setMenu(mnuAttributes);
				return;
			}

			// Determine the kind of the connected data object in order to set the proper menu
			if (selItem.getData() instanceof DataExchangeAttribute)
				treeMapping.setMenu(mnuAttribute);
			else if (selItem.getData() instanceof DataExchangeElement)
				treeMapping.setMenu(mnuElement);
		});

		treeMapping.addMouseListener(new MouseAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.events.MouseAdapter#mouseDoubleClick(org.eclipse.swt.events.MouseEvent)
			 */
			@Override
			public void mouseDoubleClick(MouseEvent e) {
				final TreeItem selItem = getSelectedMappingItem();

				if (selItem == null || selItem.getData() == null)
					return;

				if (selItem.getText().equals(ITEM_ATTRIBUTES_LABEL) || selItem.getText().equals(ITEM_ELEMENTS_LABEL))
					return;

				final boolean rootElement = selItem.getParentItem() == null;
				boolean processSingleObject = false;

				if (rootElement && singleObject)
					processSingleObject = true;

				if (selItem.getData() instanceof final DataExchangeElement element) {
					final var dlg = new EditDataExchangeElementDialog(getShell(), element, rootElement, contentType, methodType,
							processSingleObject);

					if (dlg.open() == Dialog.OK)
						selItem.setText(element.getName());
				}
				else if (selItem.getData() instanceof final DataExchangeAttribute dataExchangeAttribute) {
					final var dlg = new EditDataExchangeAttributeDialog(getShell(), dataExchangeAttribute, methodType, contentType);

					if (dlg.open() == Dialog.OK)
						selItem.setText(dataExchangeAttribute.getName());
				}
			}
		});

		// Allow data to be moved to the mapping tree
		final var dropTargetTreeMapping = new DropTarget(treeMapping, DND.DROP_MOVE | DND.DROP_COPY | DND.DROP_DEFAULT);

		// Receive the data in text format
		dropTargetTreeMapping.setTransfer(TextTransfer.getInstance());

		dropTargetTreeMapping.addDropListener(new DropTargetAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dropAccept(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dropAccept(DropTargetEvent event) {
				try {
					final TreeItem itemDrop = treeMapping.getItem(treeMapping.toControl(new Point(event.x, event.y)));

					if (itemDrop == null) {
						event.detail = DND.DROP_NONE;
						return;
					}

					if (itemDrop.getData() instanceof DataExchangeElement) {
						event.detail = DND.DROP_COPY;
						return;
					}

					event.detail = DND.DROP_NONE;
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				try {
					final TreeItem itemDrop = treeMapping.getItem(treeMapping.toControl(new Point(event.x, event.y)));

					if (itemDrop == null || itemDrop.getData() == null)
						return;

					final TreeItem selItem = domainObjectTreePanel.getSelectedItem();

					if (selItem == null || selItem.getData() == null)
						return;

					if (itemDrop.getData() instanceof DataExchangeElement) {
						if (selItem.getData() instanceof final DomainAttribute domainAttribute) {
							if (itemDrop.getText().equals(ITEM_ATTRIBUTES_LABEL)
									&& (domainAttribute.getCollectionType() == CollectionTypeEnumeration.NONE
											|| contentType == ContentTypeEnumeration.JSON))
								addNewDataExchangeAttribute(itemDrop, selItem);
							else if (itemDrop.getText().equals(ITEM_ELEMENTS_LABEL) && contentType == ContentTypeEnumeration.XML)
								addNewDataExchangeElement(itemDrop, selItem);
						}

						if (selItem.getData() instanceof final AbstractDomainAssociation assoc) {
							if (contentType == ContentTypeEnumeration.XML || contentType == ContentTypeEnumeration.JSON) {
								if (itemDrop.getText().equals(ITEM_ELEMENTS_LABEL))
									addNewDataExchangeElement(itemDrop, assoc);
							}
							else if (contentType == ContentTypeEnumeration.EXCEL97 || contentType == ContentTypeEnumeration.EXCEL2007) {
								// A new element must be joined with the root element!
								final TreeItem mappingTreeRootItem = treeMapping.getItem(0);

								if (mappingTreeRootItem != null && mappingTreeRootItem.equals(itemDrop))
									addNewDataExchangeElement(itemDrop, (AbstractDomainAssociation) selItem.getData());
							}
						}
					}
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		});

		// Allow data to be dragged from mapping tree
		final var dragSourceMapping = new DragSource(treeMapping, DND.DROP_MOVE | DND.DROP_COPY);

		// Provide the data in text format
		dragSourceMapping.setTransfer(TextTransfer.getInstance());

		dragSourceMapping.addDragListener(new DragSourceAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragStart(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragStart(DragSourceEvent event) {
				try {
					event.doit = false;

					final TreeItem selItem = getSelectedMappingItem();

					if (selItem == null || selItem.getData() == null)
						return;

					if (selItem.getData() instanceof final DataExchangeElement element) {
						if (element.isContainer())
							return;

						if (element.getMappingAttribute() == null)
							return;
					}

					if (selItem.getData() instanceof final DataExchangeAttribute attr && attr.getMappingAttribute() == null)
						return;

					event.doit = true;
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DragSourceAdapter#dragSetData(org.eclipse.swt.dnd.DragSourceEvent)
			 */
			@Override
			public void dragSetData(DragSourceEvent event) {
				try {
					final TreeItem selItem = getSelectedMappingItem();

					if (selItem == null || selItem.getData() == null)
						return;

					event.data = DND_SOURCE_MAPPING;
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		});

		final var panAssocControllers = new Composite(sashFormMapping, SWT.NONE);
		panAssocControllers.setLayout(new GridLayout());

		lblAssocController = new Label(panAssocControllers, SWT.NONE);
		lblAssocController.setText("Overwrite default handling of selected associations:");

		treeAssocController = new Tree(panAssocControllers, SWT.BORDER);
		treeAssocController.setLayoutData(new GridData(SWT.FILL, SWT.FILL, true, true));

		treeAssocController.addMenuDetectListener(e -> {
			treeAssocController.setMenu(null);

			final TreeItem selItem = getSelectedControllerItem();

			if (selItem == null || selItem.getData() == null)
				return;

			if (selItem.getData() instanceof AssociationController && !selItem.getText().equals(TREE_ITEM_LABEL_CONTR_PERSIST)
					&& !selItem.getText().equals(TREE_ITEM_LABEL_CONTR_QUERY)) {
				treeAssocController.setMenu(mnuController);
				return;
			}

			if (selItem.getData() instanceof ExchangeMappingAttribute) {
				treeAssocController.setMenu(mnuControllerAttr);
			}
		});

		// Allow data to be moved to the association controller tree
		final var dropTargetTreeController = new DropTarget(treeAssocController, DND.DROP_MOVE | DND.DROP_COPY | DND.DROP_DEFAULT);

		// Receive the data in text format
		dropTargetTreeController.setTransfer(TextTransfer.getInstance());

		dropTargetTreeController.addDropListener(new DropTargetAdapter() {
			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dragEnter(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dragEnter(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#dropAccept(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void dropAccept(DropTargetEvent event) {
				event.detail = DND.DROP_COPY;
			}

			/*
			 * (non-Javadoc)
			 * @see org.eclipse.swt.dnd.DropTargetAdapter#drop(org.eclipse.swt.dnd.DropTargetEvent)
			 */
			@Override
			public void drop(DropTargetEvent event) {
				try {
					final Object dragSourceObj = event.data;

					if (!(dragSourceObj instanceof final String dragSource))
						return;

					final TreeItem itemDrop = treeAssocController.getItem(treeAssocController.toControl(new Point(event.x, event.y)));
					final TreeItem selAssocItem = domainObjectTreePanel.getSelectedItem();
					final TreeItem selMappingItem = getSelectedMappingItem();

					if (dragSource.equals(DND_SOURCE_MAPPING)) {
						if (selMappingItem == null || selMappingItem.getData() == null)
							return;

						ExchangeMappingAttribute selMappingAttr = null;

						if (selMappingItem.getData() instanceof final DataExchangeAttribute attr)
							selMappingAttr = attr.getMappingAttribute();

						if (selMappingItem.getData() instanceof final DataExchangeElement element && !element.isContainer()
								&& element.getMappingAttribute() != null)
							selMappingAttr = element.getMappingAttribute();

						if (selMappingAttr == null)
							return;

						if (itemDrop == null) {
							final AbstractDomainAssociation assoc = selMappingAttr.getAssociation();

							if (assoc == null)
								return;

							final TreeItem controllerItem = addAssociationController(assoc);

							if (controllerItem != null)
								addMappingAttributeToController(controllerItem, selMappingAttr, (AssociationController) controllerItem.getData());

							return;
						}

						if (itemDrop.getData() instanceof final AssociationController assocController
								&& (itemDrop.getText().equals(TREE_ITEM_LABEL_CONTR_PERSIST)
										|| itemDrop.getText().equals(TREE_ITEM_LABEL_CONTR_QUERY)))
							addMappingAttributeToController(itemDrop, selMappingAttr, assocController);

						return;
					}

					if (selAssocItem == null || selAssocItem.getData() == null
							|| !(selAssocItem.getData() instanceof final AbstractDomainAssociation assoc) || itemDrop != null)
						return;

					addAssociationController(assoc);
				}
				catch (final Exception e) {
					CodeCadenzaUserInterfacePlugin.getInstance().logError(e);
				}
			}
		});

		sashForm.setWeights(1, 1);
		sashFormMapping.setWeights(70, 30);

		if (domainObject != null)
			refresh();

		initializeMenu();
	}

	/**
	 * @return the selected mapping tree item or null if no item is selected
	 */
	private TreeItem getSelectedMappingItem() {
		return Arrays.asList(treeMapping.getSelection()).stream().findFirst().orElse(null);
	}

	/**
	 * @return the selected association controller tree item or null if no item is selected
	 */
	private TreeItem getSelectedControllerItem() {
		return Arrays.asList(treeAssocController.getSelection()).stream().findFirst().orElse(null);
	}

}
