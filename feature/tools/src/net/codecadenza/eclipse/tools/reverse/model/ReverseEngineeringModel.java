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
package net.codecadenza.eclipse.tools.reverse.model;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.DBTable;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainNamespace;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.EnumAssociation;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.java.JavaEnum;
import net.codecadenza.eclipse.model.java.JavaFactory;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.java.Namespace;
import net.codecadenza.eclipse.model.project.Project;

/**
 * <p>
 * The reverse engineering model contains all domain objects and enumerations that are necessary for the reverse engineering
 * process
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class ReverseEngineeringModel {
	private final List<RevEngDomainObject> domainObjects = new ArrayList<>();
	private final List<RevEngEnum> enumerations = new ArrayList<>();
	private final Set<Namespace> namespaces = new HashSet<>();

	/**
	 * Default constructor
	 */
	public ReverseEngineeringModel() {
	}

	/**
	 * Constructor
	 * @param defaultNamespace
	 */
	public ReverseEngineeringModel(Namespace defaultNamespace) {
		final Project project = defaultNamespace.getProject();

		project.getDomainNamespace().getChildNamespaces().forEach(ns -> {
			final Namespace namespaceCopy = JavaFactory.eINSTANCE.createNamespace();
			namespaceCopy.setName(ns.getName());

			namespaces.add(namespaceCopy);

			// Add existing domain objects and enumerations to the reverse engineering model
			final var domainNamespace = (DomainNamespace) ns;

			domainNamespace.getDomainObjects().forEach(domainObject -> {
				final RevEngDomainObject obj = addDomainObject(ns.getName(), domainObject, false);

				domainObject.getAllAttributes().forEach(attr -> obj.addAttribute(attr, false));

				domainObject.getAllAssociations().forEach(assoc -> obj.addAssociation(assoc, false));
			});

			domainNamespace.getEnumerations().forEach(javaEnum -> addEnumeration(ns.getName(), javaEnum, false));
		});
	}

	/**
	 * Add a domain object
	 * @param packageName
	 * @param domainObject
	 * @param createdByReverseEngineering
	 * @return the created container object
	 */
	public RevEngDomainObject addDomainObject(String packageName, DomainObject domainObject, boolean createdByReverseEngineering) {
		final var obj = new RevEngDomainObject(this, packageName, domainObject, createdByReverseEngineering);
		domainObjects.add(obj);

		return obj;
	}

	/**
	 * Add an enumeration
	 * @param packageName
	 * @param javaEnum
	 * @param createdByReverseEngineering
	 */
	public void addEnumeration(String packageName, JavaEnum javaEnum, boolean createdByReverseEngineering) {
		enumerations.add(new RevEngEnum(packageName, javaEnum, createdByReverseEngineering));
	}

	/**
	 * @return a list of all domain objects that belong to this model
	 */
	public List<RevEngDomainObject> getDomainObjects() {
		return domainObjects;
	}

	/**
	 * @return a list of all enumerations that belong to this model
	 */
	public List<RevEngEnum> getEnumerations() {
		return enumerations;
	}

	/**
	 * @return a list with all namespaces
	 */
	public Set<Namespace> getNamespaces() {
		return namespaces;
	}

	/**
	 * Search a reverse engineering domain object by using the given domain object
	 * @param domainObject
	 * @return the reverse engineering container
	 */
	public RevEngDomainObject searchRevEngObjectByDomainObject(DomainObject domainObject) {
		return domainObjects.stream().filter(obj -> obj.getDomainObject().equals(domainObject)).findFirst().orElse(null);
	}

	/**
	 * @param revEngAssoc
	 */
	public void deleteDomainAssociation(RevEngDomainAssociation revEngAssoc) {
		final AbstractDomainAssociation assoc = revEngAssoc.getAssociation();
		final RevEngDomainObject revEngDomainObject = revEngAssoc.getParentObject();
		DBColumn colToDelete = null;

		if (revEngAssoc.isManyToOne()) {
			final var mto = (ManyToOneAssociation) assoc;
			final RevEngDomainAssociation reverseAssoc = revEngAssoc.getReverseOneToManyAssoc();
			colToDelete = mto.getColumn();

			if (reverseAssoc != null) {
				final RevEngDomainObject reverseObj = reverseAssoc.getParentObject();
				reverseObj.getAssociations().remove(reverseAssoc);
			}
		}
		else if (revEngAssoc.isOneToOne()) {
			final var oto = (OneToOneAssociation) assoc;
			final RevEngDomainAssociation reverseAssoc = revEngAssoc.getReverseOneToOneAssoc();

			// Delete the respective column if the association represents the owner
			if (oto.isOwner())
				colToDelete = oto.getColumn();

			if (reverseAssoc != null && oto.isOwner()) {
				final RevEngDomainObject reverseObj = reverseAssoc.getParentObject();
				reverseObj.getAssociations().remove(reverseAssoc);
			}
		}
		else if (revEngAssoc.isManyToMany()) {
			final var mtm = (ManyToManyAssociation) assoc;
			final RevEngDomainAssociation reverseAssoc = revEngAssoc.getReverseManyToManyAssoc();

			if (reverseAssoc != null && mtm.isOwner()) {
				final RevEngDomainObject reverseObj = reverseAssoc.getParentObject();
				reverseObj.getAssociations().remove(reverseAssoc);
			}
		}
		else if (revEngAssoc.isOneToMany()) {
			final RevEngDomainAssociation reverseAssoc = revEngAssoc.getReverseManyToOneAssoc();

			if (reverseAssoc != null)
				reverseAssoc.getAssociation().setOwner(true);
		}

		revEngDomainObject.getAssociations().remove(revEngAssoc);

		if (colToDelete != null)
			deleteColumn(colToDelete);
	}

	/**
	 * @param revEngAttribute
	 */
	public void deleteDomainAttribute(RevEngDomainAttribute revEngAttribute) {
		final DomainAttribute attribute = revEngAttribute.getDomainAttribute();
		final RevEngDomainObject revEngObj = revEngAttribute.getParentObject();
		final DBColumn column = attribute.getColumn();
		final DomainObject domainObject = revEngObj.getDomainObject();
		final JavaType attrType = attribute.getJavaType();

		// If the type is an enum the corresponding enum association must be removed also!
		if (attrType.isEnum()) {
			final EnumAssociation enumAssocToDelete = domainObject.getEnumAssociations().stream()
					.filter(enumAssoc -> enumAssoc.getTarget().equals(attrType)).findFirst().orElse(null);

			if (enumAssocToDelete != null)
				domainObject.getEnumAssociations().remove(enumAssocToDelete);
		}

		// Delete the column the attribute is mapped to
		deleteColumn(column);

		revEngObj.getAttributes().remove(revEngAttribute);
	}

	/**
	 * Delete the column. All foreign keys and all indexes that reference the column must be deleted also!
	 * @param column
	 */
	private void deleteColumn(DBColumn column) {
		final DBTable table = column.getDatabaseTable();

		if (table == null)
			return;

		final var fkList = new ArrayList<ForeignKey>();
		final var indexList = new ArrayList<DBIndex>();

		for (final ForeignKey foreignKey : table.getForeignKeys())
			if (foreignKey.getColumn().equals(column))
				fkList.add(foreignKey);

		for (final DBIndex index : table.getIndexes())
			if (index.getColumns().contains(column))
				indexList.add(index);

		table.getForeignKeys().removeAll(fkList);
		table.getIndexes().removeAll(indexList);
		table.getColumns().remove(column);
	}

}
