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
package net.codecadenza.eclipse.testing.domain;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.testing.dialog.client.CreateDefaultFormsDialog;

/**
 * <p>
 * Objects of this class represent domain objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObject {
	private static final String REPOSITORY_SUFFIX = "Repository";
	private static final String EXCHANGE_SERVICE_SUFFIX = "ExchangeService";
	private static final String TABLE_NAME_SUFFIX = "_tab";

	private final String name;
	private final List<DomainAttribute> attributes = new ArrayList<>();
	private final List<DomainAssociation> associations = new ArrayList<>();
	private final List<EnumAssociation> enumAssociations = new ArrayList<>();
	private String namespace = Project.DEFAULT_NAMESPACE;
	private boolean mappedSuperClass;
	private DomainObject parentDomainObject;
	private int startXPosition;
	private int startYPosition;
	private boolean createDefaultForms;
	private String label;
	private String pluralLabel;
	private String tableName;

	/**
	 * Constructor
	 * @param name
	 */
	public DomainObject(String name) {
		this.name = name;
		this.label = name.toLowerCase();
		this.pluralLabel = name + "s";
		this.tableName = name.toLowerCase() + TABLE_NAME_SUFFIX;
	}

	/**
	 * @return the name of the domain object
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param domainAttribute
	 */
	public void addAttribute(DomainAttribute domainAttribute) {
		attributes.add(domainAttribute);
	}

	/**
	 * @param domainAssociation
	 */
	public void addAssociation(DomainAssociation domainAssociation) {
		associations.add(domainAssociation);
	}

	/**
	 * @param enumAssociation
	 */
	public void addEnumAssociation(EnumAssociation enumAssociation) {
		enumAssociations.add(enumAssociation);
	}

	/**
	 * @return the list with all attributes that belong to this domain object
	 */
	public List<DomainAttribute> getAttributes() {
		return attributes;
	}

	/**
	 * @return the list with all attributes including further attributes based on all enumeration associations
	 */
	public List<DomainAttribute> getAllAttributes() {
		final var allAttributes = new ArrayList<DomainAttribute>();

		if (getParentDomainObject() != null)
			allAttributes.addAll(parentDomainObject.getAllAttributes());

		allAttributes.addAll(attributes);

		for (final EnumAssociation enumAssoc : enumAssociations) {
			final var enumAttribute = new DomainAttribute(enumAssoc.getName(), enumAssoc.getTarget().getName(), false, false);

			allAttributes.add(enumAttribute);
		}

		return allAttributes;
	}

	/**
	 * @return a list with all associations
	 */
	public List<DomainAssociation> getAssociations() {
		return associations;
	}

	/**
	 * @return a list with all enumeration associations
	 */
	public List<EnumAssociation> getEnumAssociations() {
		return enumAssociations;
	}

	/**
	 * @return the namespace
	 */
	public String getNamespace() {
		return namespace;
	}

	/**
	 * @param namespace
	 */
	public void setNamespace(String namespace) {
		this.namespace = namespace;
	}

	/**
	 * @return true if the domain object represents a mapped superclass
	 */
	public boolean isMappedSuperClass() {
		return mappedSuperClass;
	}

	/**
	 * @param mappedSuperClass
	 */
	public void setMappedSuperClass(boolean mappedSuperClass) {
		this.mappedSuperClass = mappedSuperClass;
	}

	/**
	 * @return the parent domain object
	 */
	public DomainObject getParentDomainObject() {
		return parentDomainObject;
	}

	/**
	 * @param parentDomainObject
	 */
	public void setParentDomainObject(DomainObject parentDomainObject) {
		this.parentDomainObject = parentDomainObject;
	}

	/**
	 * @param startXPosition
	 */
	public void setStartXPosition(int startXPosition) {
		this.startXPosition = startXPosition;
	}

	/**
	 * @return the X-position of the domain object in the graphical editor
	 */
	public int getStartXPosition() {
		return startXPosition;
	}

	/**
	 * @param startYPosition
	 */
	public void setStartYPosition(int startYPosition) {
		this.startYPosition = startYPosition;
	}

	/**
	 * @return the Y-position of the domain object in the graphical editor
	 */
	public int getStartYPosition() {
		return startYPosition;
	}

	/**
	 * @return true if the client forms should be created by using the {@link CreateDefaultFormsDialog}
	 */
	public boolean isCreateDefaultForms() {
		return createDefaultForms;
	}

	/**
	 * @param createDefaultForms
	 */
	public void setCreateDefaultForms(boolean createDefaultForms) {
		this.createDefaultForms = createDefaultForms;
	}

	/**
	 * @return the name of the corresponding data exchange service
	 */
	public String getDataExchangeServiceName() {
		return name + EXCHANGE_SERVICE_SUFFIX;
	}

	/**
	 * @return the default name of the dialog for creating a new object
	 */
	public String getCreateFormName() {
		return "CreateNew" + name + "Dialog";
	}

	/**
	 * @return the default name of the dialog for editing an object
	 */
	public String getEditFormName() {
		return "Edit" + name + "Dialog";
	}

	/**
	 * @return the default name of the dialog for opening an object in readonly mode
	 */
	public String getReadonlyFormName() {
		return "View" + name + "Dialog";
	}

	/**
	 * @return the default name of the respective view
	 */
	public String getViewFormName() {
		return name + "View";
	}

	/**
	 * @return the default name of the respective list-of-values view
	 */
	public String getListOfValuesName() {
		return name + "LOV";
	}

	/**
	 * @return the default name of the respective tree view
	 */
	public String getTreeViewName() {
		return name + "TreeView";
	}

	/**
	 * @return the name of the respective GUI test case
	 */
	public String getTestCaseName() {
		return name + "Test";
	}

	/**
	 * @return the label
	 */
	public String getLabel() {
		return label;
	}

	/**
	 * @param label
	 */
	public void setLabel(String label) {
		this.label = label;
	}

	/**
	 * @return the plural form of the label
	 */
	public String getPluralLabel() {
		return pluralLabel;
	}

	/**
	 * @param pluralLabel
	 */
	public void setPluralLabel(String pluralLabel) {
		this.pluralLabel = pluralLabel;
	}

	/**
	 * @return the title of the dialog for creating a new object
	 */
	public String getCreateFormTitle() {
		return "Create new " + label;
	}

	/**
	 * @return the title of the dialog for editing an object
	 */
	public String getEditFormTitle() {
		return "Edit " + label;
	}

	/**
	 * @return the title of the dialog for opening an object in readonly mode
	 */
	public String getReadonlyFormTitle() {
		return "View " + label;
	}

	/**
	 * @return the title of the toolbar button for deleting an object
	 */
	public String getToolbarTitleDelete() {
		return "Delete selected " + label;
	}

	/**
	 * @return the title of the respective view
	 */
	public String getViewFormTitle() {
		return pluralLabel + " view";
	}

	/**
	 * @return the name of the table this domain object is mapped to
	 */
	public String getTableName() {
		return tableName;
	}

	/**
	 * @param tableName
	 */
	public void setTableName(String tableName) {
		this.tableName = tableName;
	}

	/**
	 * @return the name of the respective repository
	 */
	public String getRepositoryName() {
		return name + REPOSITORY_SUFFIX;
	}

}
