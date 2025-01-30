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
package net.codecadenza.eclipse.tools.reverse.service;

import java.util.ArrayList;
import java.util.List;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.domain.TemporalTypeEnumeration;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAssociation;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainAttribute;
import net.codecadenza.eclipse.tools.reverse.model.RevEngDomainObject;
import net.codecadenza.eclipse.tools.reverse.model.RevEngEnum;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringLogEntry;
import net.codecadenza.eclipse.tools.reverse.model.ReverseEngineeringModel;
import org.eclipse.core.runtime.IStatus;

/**
 * <p>
 * Service for domain model validation
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainModelValidationService {
	private final ReverseEngineeringModel revEngModel;
	private final List<ReverseEngineeringLogEntry> validationLog = new ArrayList<>();

	/**
	 * Constructor for validating a list of domain objects
	 * @param revEngModel
	 */
	public DomainModelValidationService(ReverseEngineeringModel revEngModel) {
		this.revEngModel = revEngModel;
	}

	/**
	 * Constructor for validating one domain object
	 * @param domainObject
	 */
	public DomainModelValidationService(DomainObject domainObject) {
		this.revEngModel = new ReverseEngineeringModel();
		this.revEngModel.addDomainObject("", domainObject, true);
	}

	/**
	 * @return a list containing all validation errors
	 */
	public List<ReverseEngineeringLogEntry> validateDomainModel() {
		revEngModel.getDomainObjects().forEach(revEngObj -> {
			final DomainObject domainObject = revEngObj.getDomainObject();
			final String domainObjectName = domainObject.getName();
			final DBTable table = domainObject.getDatabaseTable();
			boolean pkFound = false;

			validateDomainObjectNameInt(domainObjectName);
			validateDomainObjectLabelInt(domainObject.getLabel());
			validateDomainObjectNameInt(domainObject.getNamePlural());
			validateDomainObjectLabelPluralInt(domainObject.getLabelPlural());

			// Check if the domain object name is unique within the package
			checkUniqueClassName(domainObjectName);

			int displayAttrCount = 0;
			int versionAttrCount = 0;

			for (final RevEngDomainAttribute revEngAttr : revEngObj.getAttributes()) {
				final DomainAttribute attr = revEngAttr.getDomainAttribute();
				final String domainAttrName = attr.getName();

				if (attr.isPk())
					pkFound = true;

				// Check if the domain attribute name is unique
				checkUniqueFieldName(domainAttrName, revEngObj);

				if (attr.isDisplayAttribute())
					displayAttrCount++;

				if (attr.isTrackVersion())
					versionAttrCount++;

				if (attr.getColumn() == null)
					addLog("The attribute '" + domainAttrName + "' is not mapped to a column!", ReverseEngineeringLogEntry.Status.ERROR);

				if (attr.getJavaType() != null) {
					if (revEngAttr.isCreatedByReverseEngineering() && attr.getJavaType().isPrimitive() && attr.getColumn() != null
							&& attr.getColumn().isNullable())
						addLog("The attribute '" + domainAttrName + "' must not be mapped to a primitive type!",
								ReverseEngineeringLogEntry.Status.WARNING);

					if (attr.getJavaType().isDateOrCalendar() && attr.getTemporalType() == TemporalTypeEnumeration.NONE)
						addLog("The attribute '" + domainAttrName + "' must be supplied with a valid temporal type!",
								ReverseEngineeringLogEntry.Status.WARNING);
				}
				else
					addLog("A valid Java type for attribute '" + domainAttrName + "' is missing!", ReverseEngineeringLogEntry.Status.ERROR);

				validateFieldName(attr.getName());
				validateDomainAttributeLabel(attr.getLabel());
				validateDomainAttributeLabelPlural(attr.getLabelPlural());
			}

			if (!pkFound)
				addLog("The domain object '" + domainObjectName + "' has no primary key attribute!",
						ReverseEngineeringLogEntry.Status.ERROR);

			if (versionAttrCount > 1)
				addLog("The domain object '" + domainObjectName + "' has multiple version attributes!",
						ReverseEngineeringLogEntry.Status.WARNING);

			if (displayAttrCount > 1)
				addLog("The domain object '" + domainObjectName + "' has multiple display attributes!",
						ReverseEngineeringLogEntry.Status.WARNING);

			revEngObj.getAssociations().stream().map(RevEngDomainAssociation::getAssociation).forEach(assoc -> {
				final String assocName = assoc.getName();

				// Check if the association name is unique
				checkUniqueFieldName(assocName, revEngObj);

				validateFieldName(assocName);

				if (assoc.getTarget() == null)
					addLog("The target domain object for association '" + assocName + "' is missing!",
							ReverseEngineeringLogEntry.Status.ERROR);

				if (assoc instanceof final ManyToOneAssociation mto && mto.getColumn() == null)
					addLog("The association '" + assocName + "' must be mapped to a column!", ReverseEngineeringLogEntry.Status.ERROR);
				else if (assoc instanceof final OneToOneAssociation oto && oto.isOwner() && oto.getColumn() == null)
					addLog("The association '" + assocName + "' must be mapped to a column!", ReverseEngineeringLogEntry.Status.ERROR);
				else if (assoc instanceof final ManyToManyAssociation mtm && mtm.getTable() == null && mtm.isOwner())
					addLog("The association '" + assocName + "' must be mapped to a table!", ReverseEngineeringLogEntry.Status.ERROR);
				else if (assoc instanceof final OneToManyAssociation otm && !otm.isBidirectional() && otm.getTable() == null)
					addLog("The association '" + assocName + "' must be mapped to a table!", ReverseEngineeringLogEntry.Status.ERROR);
			});

			// Validate if the foreign key and the index columns are initialized properly!
			if (table != null) {
				table.getForeignKeys().forEach(foreignKey -> {
					if (foreignKey.getColumn() == null)
						addLog("The column for foreign key '" + foreignKey.getConvertedName() + "' is not set!",
								ReverseEngineeringLogEntry.Status.ERROR);

					if (foreignKey.getReferencedColumn() == null)
						addLog("The referenced column for foreign key '" + foreignKey.getConvertedName() + "' is not set!",
								ReverseEngineeringLogEntry.Status.ERROR);
				});

				for (final DBIndex index : table.getIndexes())
					if (index.getColumns().isEmpty())
						addLog("The database index '" + index.getConvertedName() + "' contains no columns!",
								ReverseEngineeringLogEntry.Status.ERROR);
			}
		});

		revEngModel.getEnumerations().forEach(revEngEnum -> checkUniqueClassName(revEngEnum.getJavaEnum().getName()));

		return validationLog;
	}

	/**
	 * @param className
	 */
	private void checkUniqueClassName(String className) {
		int count = 0;

		for (final RevEngDomainObject revEngObj : revEngModel.getDomainObjects())
			if (className.equals(revEngObj.getDomainObject().getName()))
				count++;

		for (final RevEngEnum revEngEnum : revEngModel.getEnumerations())
			if (revEngEnum.getJavaEnum().getName().equals(className))
				count++;

		if (count > 1)
			addLog("A domain object or an enumeration with the name '" + className + "' already exists!",
					ReverseEngineeringLogEntry.Status.WARNING);
	}

	/**
	 * @param name
	 * @param revEngObj
	 */
	private void checkUniqueFieldName(String name, RevEngDomainObject revEngObj) {
		final DomainObject domainObject = revEngObj.getDomainObject();
		int count = 0;

		for (final RevEngDomainAttribute revEngAttr : revEngObj.getAttributes())
			if (revEngAttr.getDomainAttribute().getName().equals(name))
				count++;

		for (final RevEngDomainAssociation revEngAssoc : revEngObj.getAssociations())
			if (revEngAssoc.getAssociation().getName().equals(name))
				count++;

		if (count > 1)
			addLog("At least two fields of domain object '" + domainObject.getName() + "' have the same name '" + name + "'!",
					ReverseEngineeringLogEntry.Status.WARNING);
	}

	/**
	 * Create a new log entry
	 * @param message
	 * @param status
	 */
	private void addLog(String message, ReverseEngineeringLogEntry.Status status) {
		if (message == null || message.isEmpty())
			return;

		validationLog.add(new ReverseEngineeringLogEntry(message, status, ReverseEngineeringLogEntry.Source.VALIDATION));
	}

	/**
	 * Validate the domain object name
	 * @param name
	 * @return an error message if the validation has failed
	 */
	public static String validateDomainObjectName(String name) {
		final IStatus status = EclipseIDEService.validateJavaTypeName(name);

		if (status.getSeverity() > IStatus.INFO)
			return status.getMessage();

		return null;
	}

	/**
	 * Validate the class name
	 * @param name
	 */
	private void validateDomainObjectNameInt(String name) {
		addLog(validateDomainObjectName(name), ReverseEngineeringLogEntry.Status.WARNING);
	}

	/**
	 * Validate the domain object label
	 * @param label
	 * @return an error message if the validation has failed
	 */
	public static String validateDomainObjectLabel(String label) {
		if (label.isEmpty())
			return "The label for a domain object must not be empty!";

		return null;
	}

	/**
	 * Validate the domain object label
	 * @param label
	 */
	private void validateDomainObjectLabelInt(String label) {
		addLog(validateDomainObjectLabel(label), ReverseEngineeringLogEntry.Status.WARNING);
	}

	/**
	 * Validate the plural form of the label
	 * @param label
	 * @return an error message if the validation has failed
	 */
	public static String validateDomainObjectLabelPlural(String label) {
		if (label.isEmpty())
			return "The plural form of the label for a domain object must not be empty!";

		return null;
	}

	/**
	 * Validate the plural form of the label
	 * @param label
	 */
	private void validateDomainObjectLabelPluralInt(String label) {
		addLog(validateDomainObjectLabelPlural(label), ReverseEngineeringLogEntry.Status.WARNING);
	}

	/**
	 * Validate the field name
	 * @param name
	 */
	private void validateFieldName(String name) {
		final IStatus status = EclipseIDEService.validateFieldName(name);

		if (status.getSeverity() > IStatus.INFO)
			addLog(status.getMessage(), ReverseEngineeringLogEntry.Status.WARNING);
	}

	/**
	 * Validate the domain attribute label
	 * @param label
	 */
	private void validateDomainAttributeLabel(String label) {
		if (label.isEmpty())
			addLog("The label for a domain attribute must not be empty!", ReverseEngineeringLogEntry.Status.WARNING);
	}

	/**
	 * Validate the plural form of a domain attribute label
	 * @param label
	 */
	private void validateDomainAttributeLabelPlural(String label) {
		if (label.isEmpty())
			addLog("The plural form of the label for a domain attribute must not be empty!", ReverseEngineeringLogEntry.Status.WARNING);
	}

}
