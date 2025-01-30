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
package net.codecadenza.eclipse.service.domain;

import net.codecadenza.eclipse.generator.client.imp.angular.module.AngularAppModuleGenerator;
import net.codecadenza.eclipse.generator.client.imp.angular.module.AngularFormModuleGenerator;
import net.codecadenza.eclipse.generator.domain.CallbackListenerGenerator;
import net.codecadenza.eclipse.generator.domain.DomainMetaModelGenerator;
import net.codecadenza.eclipse.generator.domain.DomainObjectGenerator;
import net.codecadenza.eclipse.model.boundary.BoundaryBean;
import net.codecadenza.eclipse.model.boundary.BoundaryMethod;
import net.codecadenza.eclipse.model.client.Form;
import net.codecadenza.eclipse.model.db.DBColumn;
import net.codecadenza.eclipse.model.db.DBIndex;
import net.codecadenza.eclipse.model.db.Database;
import net.codecadenza.eclipse.model.db.ForeignKey;
import net.codecadenza.eclipse.model.domain.AbstractDomainAssociation;
import net.codecadenza.eclipse.model.domain.DomainAttribute;
import net.codecadenza.eclipse.model.domain.DomainObject;
import net.codecadenza.eclipse.model.domain.ManyToManyAssociation;
import net.codecadenza.eclipse.model.domain.ManyToOneAssociation;
import net.codecadenza.eclipse.model.domain.OneToManyAssociation;
import net.codecadenza.eclipse.model.domain.OneToOneAssociation;
import net.codecadenza.eclipse.model.dto.DTOBean;
import net.codecadenza.eclipse.model.dto.DTOBeanAttribute;
import net.codecadenza.eclipse.model.exchange.AssociationController;
import net.codecadenza.eclipse.model.exchange.DataExchangeAttribute;
import net.codecadenza.eclipse.model.exchange.DataExchangeElement;
import net.codecadenza.eclipse.model.exchange.DataExchangeMethod;
import net.codecadenza.eclipse.model.exchange.DataExchangeServiceBean;
import net.codecadenza.eclipse.model.exchange.ExchangeMappingObject;
import net.codecadenza.eclipse.model.exchange.FilterMethodParameter;
import net.codecadenza.eclipse.model.java.MethodParameter;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.repository.RepositoryMethod;
import net.codecadenza.eclipse.model.repository.RepositoryMethodParameter;
import net.codecadenza.eclipse.service.build.ProjectBuildFactory;
import net.codecadenza.eclipse.tools.ide.EclipseIDEService;
import org.eclipse.emf.common.util.BasicEList;

/**
 * <p>
 * Service for domain objects
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class DomainObjectService {
	private final Project project;

	/**
	 * Constructor
	 * @param project
	 */
	public DomainObjectService(Project project) {
		this.project = project;
	}

	/**
	 * Rebuild the domain object source files
	 * @param domainObject
	 * @param rebuildPersistenceXML
	 * @throws Exception if either the domain object source file or the persistence.xml could not be rebuilt
	 */
	public void rebuildDomainObjectSourceFiles(DomainObject domainObject, boolean rebuildPersistenceXML) throws Exception {
		if (rebuildPersistenceXML)
			ProjectBuildFactory.getBuildService(project).rebuildPersistenceUnit();

		if (domainObject.getRootParentDomainObject(true).getPKAttribute() == null)
			return;

		// Rebuild the domain object
		final var domainObjectGenerator = new DomainObjectGenerator(domainObject);
		domainObjectGenerator.createSourceFile();

		// Rebuild the canonical meta-model
		new DomainMetaModelGenerator(domainObject).createSourceFile();

		// If necessary, rebuild the callback listener
		if (domainObjectGenerator.isAddListener())
			new CallbackListenerGenerator(domainObject).createSourceFile();
	}

	/**
	 * Remove the given domain association
	 * @param assoc
	 * @throws Exception if either the domain object source file or the persistence.xml could not be rebuilt
	 */
	public void removeDomainObjectAssociation(AbstractDomainAssociation assoc) throws Exception {
		final DomainObject source = assoc.getDomainObject();
		final DomainObject target = assoc.getTarget();

		checkAssociation(assoc);

		if (assoc instanceof final ManyToManyAssociation mtm) {
			if (mtm.isBidirectional()) {
				final AbstractDomainAssociation reverseAssoc = mtm.getReverseAssociation();
				final DomainObject targetDomainObject = mtm.getTarget();

				checkAssociation(reverseAssoc);

				targetDomainObject.getAssociations().remove(reverseAssoc);
			}

			removeManyToManyAssociationTable(mtm);
		}
		else if (assoc instanceof final ManyToOneAssociation mto)
			removeManyToOneAssociationColumn(mto);
		else if (assoc instanceof final OneToOneAssociation oto) {
			if (oto.isBidirectional()) {
				final AbstractDomainAssociation reverseAssoc = oto.getReverseAssociation();
				final DomainObject targetDomainObject = oto.getTarget();

				checkAssociation(reverseAssoc);

				targetDomainObject.getAssociations().remove(reverseAssoc);
			}

			removeOneToOneAssociationColumn(oto);
		}
		else if (assoc instanceof final OneToManyAssociation otm) {
			if (otm.isBidirectional()) {
				final ManyToOneAssociation reverseAssoc = otm.getReverseAssociation();
				final DomainObject targetDomainObject = otm.getTarget();

				checkAssociation(reverseAssoc);

				removeManyToOneAssociationColumn(reverseAssoc);
				targetDomainObject.getAssociations().remove(reverseAssoc);
			}
			else
				removeOneToManyAssociationTable(otm);
		}

		rebuildDomainObjectSourceFiles(source, false);
		rebuildDomainObjectSourceFiles(target, false);
	}

	/**
	 * Remove the given domain attribute
	 * @param attribute
	 * @throws IllegalStateException if the attribute must not be removed as it is referenced by other objects of the internal
	 *           meta-model
	 * @throws Exception if either the domain object source file or the persistence.xml could not be rebuilt
	 */
	public void removeDomainAttribute(DomainAttribute attribute) throws Exception {
		final DomainObject bean = attribute.getDomainObject();

		// Test if the attribute may be deleted!
		checkAttribute(attribute);

		// Remove respective indexes from the table
		if (attribute.isPersistent()) {
			removeIndexesAndForeignKeysOfColumn(attribute.getColumn());
			attribute.getColumn().getDatabaseTable().getColumns().remove(attribute.getColumn());
		}

		attribute.getDomainObject().getAttributes().remove(attribute);

		rebuildDomainObjectSourceFiles(bean, false);
	}

	/**
	 * Remove the given domain object
	 * @param domainObject
	 * @throws IllegalStateException if the domain object cannot be removed as internal validation checks has failed
	 * @throws Exception if one of the corresponding files could not be removed
	 */
	public void removeDomainObject(DomainObject domainObject) throws Exception {
		final var msgPrefix = "The domain object cannot be removed, because ";

		// Check if this class represents a mapped superclass for other domain objects
		if (domainObject.isMappedSuperClass() && !domainObject.getTargetInheritances().isEmpty())
			throw new IllegalStateException(msgPrefix + "it is a mapped superclass for other domain objects!");

		// Check if this class represents a superclass for other domain objects
		if (domainObject.getInheritance() != null)
			for (final DomainObject b : project.getAllDomainObjectsOfProject(true, true))
				if (b.getParent() != null && b.getParent().equals(domainObject))
					throw new IllegalStateException(msgPrefix + "it is a superclass for domain object '" + b.getName() + "'!");

		// Search for associations
		if (!domainObject.getAssociations().isEmpty())
			throw new IllegalStateException(msgPrefix + "of existing associations!");

		for (final DomainObject b : project.getAllDomainObjectsOfProject(true, true)) {
			if (b.equals(domainObject))
				continue;

			for (final AbstractDomainAssociation assoc : b.getAssociations())
				if (assoc.getTarget().equals(domainObject))
					throw new IllegalStateException(msgPrefix + "of existing associations!");
		}

		// Check if existing forms use this domain object. We don't check grid panels here as the DTO check should be sufficient for
		// that!
		for (final Form f : project.getAllFormsOfProject())
			if (f.getDomainObject().equals(domainObject))
				throw new IllegalStateException(msgPrefix + "form '" + f.getName() + "' directly depends on it!");

		// Check if existing data transfer objects use this domain object
		for (final DTOBean b : project.getAllDTOsOfProject())
			if (b.getDomainObject().equals(domainObject))
				throw new IllegalStateException(msgPrefix + "it is referenced by '" + b.getName() + "'!");

		// Check if existing boundary beans use this domain object
		if (project.getBoundaryByDomainObject(domainObject) != null)
			throw new IllegalStateException(
					msgPrefix + "it is referenced by '" + project.getBoundaryByDomainObject(domainObject).getName() + "'!");

		// Check if existing exchange service beans use this domain object
		if (!project.getAllExchangeServicesOfDomainObject(domainObject).isEmpty()) {
			final DataExchangeServiceBean exchangeService = project.getAllExchangeServicesOfDomainObject(domainObject).get(0);
			throw new IllegalStateException(
					msgPrefix + "it is referenced by data exchange service '" + exchangeService.getName() + "'!");
		}

		// Check if existing mapping objects use this domain object
		if (!project.getAllMappingObjectsOfDomainObject(domainObject).isEmpty()) {
			final ExchangeMappingObject mappingObject = project.getAllMappingObjectsOfDomainObject(domainObject).get(0);
			throw new IllegalStateException(msgPrefix + "it is referenced by mapping object '" + mappingObject.getName() + "'!");
		}

		if (project.isBoundaryMode()) {
			// If the project uses facades it can be assumed that the repository will be deleted automatically with the respective
			// boundary!
			for (final Repository repository : project.getAllRepositoriesOfProject())
				if (repository.getDomainObject().equals(domainObject))
					throw new IllegalStateException(msgPrefix + "it is referenced by '" + repository.getName() + "'!");
		}

		// Delete the database table
		if (domainObject.getDatabaseTable() != null) {
			final Database database = domainObject.getDatabaseTable().getDatabase();
			database.getDatabaseTables().remove(domainObject.getDatabaseTable());
			project.eResource().getContents().remove(domainObject.getDatabaseTable());
		}

		removeDomainObjectSourceFiles(domainObject);
	}

	/**
	 * Remove the domain object source files
	 * @param domainObject
	 * @throws Exception if one of the corresponding files could not be removed
	 */
	public void removeDomainObjectSourceFiles(DomainObject domainObject) throws Exception {
		// Delete the domain object file
		EclipseIDEService.deleteSource(domainObject.getSourceFile());

		// Delete the canonical meta-model class
		EclipseIDEService.deleteSource(domainObject.getMetaModelSourceFile());

		// Delete the listener class
		EclipseIDEService.deleteSource(domainObject.getListenerSourceFile());

		if (project.hasAngularClient()) {
			// Remove the corresponding form module
			final WorkspaceFile formModule = new AngularFormModuleGenerator(domainObject).getSourceFile();

			EclipseIDEService.deleteWorkspaceFile(formModule);

			// Rebuild the main module
			new AngularAppModuleGenerator(project).createSourceFile();
		}
	}

	/**
	 * Check if the given association can be safely removed
	 * @param assoc
	 * @throws IllegalStateException if the check has failed
	 */
	private void checkAssociation(AbstractDomainAssociation assoc) {
		final var msgPrefix = "The association cannot be removed, because it is referenced ";

		for (final DTOBean bean : project.getAllDTOsOfProject())
			for (final DTOBeanAttribute attr : bean.getAttributes()) {
				if (attr.getAssociation() != null && attr.getAssociation().equals(assoc))
					throw new IllegalStateException(msgPrefix + "in DTO '" + bean.getName() + "'!");

				for (final AbstractDomainAssociation a : attr.getAssociationList())
					if (a.equals(assoc))
						throw new IllegalStateException(msgPrefix + "in DTO '" + bean.getName() + "'!");
			}

		// It might be the case that this association is used by a repository method!
		for (final Repository repository : project.getAllRepositoriesOfProject())
			for (final RepositoryMethod method : repository.getRepositoryMethods())
				for (final MethodParameter param : method.getMethodParameters()) {
					if (!(param instanceof final RepositoryMethodParameter repoParam))
						continue;

					if (repoParam.getAssociation() != null && repoParam.getAssociation().equals(assoc))
						throw new IllegalStateException(
								msgPrefix + "by method '" + method.getName() + "()' in class '" + repository.getName() + "'!");
				}

		// It might be the case that this association is used by a boundary method
		for (final BoundaryBean b : project.getAllBoundariesOfProject())
			for (final BoundaryMethod m : b.getBoundaryMethods())
				if (m.getAssociation() != null && m.getAssociation().equals(assoc))
					throw new IllegalStateException(msgPrefix + "by method '" + m.getName() + "()' in class '" + b.getName() + "'!");

		// Test if objects of data exchange services are referring to this association!
		for (final DataExchangeServiceBean b : project.getAllExchangeServices()) {
			for (final DataExchangeMethod m : b.getDataExchangeMethods()) {
				if (m.getRootElement() == null)
					continue;

				for (final DataExchangeElement e : m.getRootElement().getAllElements()) {
					if (e.getMappingAttribute() != null && e.getMappingAttribute().getAssociation() != null
							&& (e.getMappingAttribute().getAssociation().equals(assoc)
									|| e.getMappingAttribute().getAssociationList().contains(assoc)))
						throw new IllegalStateException(
								msgPrefix + "by a mapping attribute of method '" + m.getName() + "()' in class '" + b.getName() + "'!");

					for (final DataExchangeAttribute a : e.getAttributes())
						if (a.getMappingAttribute() != null && a.getMappingAttribute().getAssociation() != null
								&& (a.getMappingAttribute().getAssociation().equals(assoc)
										|| a.getMappingAttribute().getAssociationList().contains(assoc))) {
							final var msg = msgPrefix + "by a mapping attribute of method '" + m.getName() + "()' in class '" + b.getName()
									+ "'!";

							throw new IllegalStateException(msg);
						}
				}

				for (final MethodParameter p : m.getMethodParameters()) {
					if (p instanceof final FilterMethodParameter filterParam && filterParam.getAssociation() != null
							&& (filterParam.getAssociation().equals(assoc) || filterParam.getAssociationList().contains(assoc))) {
						final var msg = msgPrefix + "by a filter parameter of method '" + m.getName() + "()' in class '" + b.getName() + "'!";

						throw new IllegalStateException(msg);
					}
				}

				for (final AssociationController c : m.getAssociationControllers())
					if (c.getAssociation().equals(assoc)) {
						final var msg = msgPrefix + "by an association controller of method '" + m.getName() + "()' in class '" + b.getName()
								+ "'!";

						throw new IllegalStateException(msg);
					}
			}
		}
	}

	/**
	 * Check if the given attribute can be safely removed
	 * @param attribute
	 * @throws IllegalStateException if the check has failed
	 */
	private void checkAttribute(DomainAttribute attribute) {
		final var msgPrefix = "The attribute cannot be removed, because it is referenced ";

		if (attribute.isPk())
			throw new IllegalStateException("An attribute which is defined as primary key cannot be removed!");

		// Test if the attribute is used in DTOs
		for (final DTOBean bean : project.getAllDTOsOfProject())
			for (final DTOBeanAttribute attr : bean.getAttributes())
				if (attr.getDomainAttribute() != null && attr.getDomainAttribute().equals(attribute))
					throw new IllegalStateException(msgPrefix + "in DTO '" + bean.getName() + "'!");

		// It might be the case that this attribute is used by a repository method!
		for (final Repository repository : project.getAllRepositoriesOfProject())
			for (final RepositoryMethod method : repository.getRepositoryMethods())
				for (final MethodParameter param : method.getMethodParameters()) {
					if (!(param instanceof final RepositoryMethodParameter repoParam))
						continue;

					if (repoParam.getAttribute() != null && repoParam.getAttribute().equals(attribute))
						throw new IllegalStateException(
								msgPrefix + "by method '" + method.getName() + "()' in class '" + repository.getName() + "'!");
				}

		// It might be the case that this attribute is used by a boundary method
		for (final BoundaryBean b : project.getAllBoundariesOfProject())
			for (final BoundaryMethod m : b.getBoundaryMethods())
				if (m.getDomainAttribute() != null && m.getDomainAttribute().equals(attribute))
					throw new IllegalStateException(msgPrefix + "by method '" + m.getName() + "()' in class '" + b.getName() + "'!");

		// Test if objects of data exchange services are referring to this attribute!
		for (final DataExchangeServiceBean b : project.getAllExchangeServices()) {
			for (final DataExchangeMethod m : b.getDataExchangeMethods()) {
				if (m.getRootElement() == null)
					continue;

				for (final DataExchangeElement e : m.getRootElement().getAllElements()) {
					if (e.getMappingAttribute() != null && e.getMappingAttribute().getDomainAttribute() != null
							&& e.getMappingAttribute().getDomainAttribute().equals(attribute))
						throw new IllegalStateException(
								msgPrefix + "by a mapping attribute of method '" + m.getName() + "()' in class '" + b.getName() + "'!");

					for (final DataExchangeAttribute a : e.getAttributes())
						if (a.getMappingAttribute() != null && a.getMappingAttribute().getDomainAttribute() != null
								&& a.getMappingAttribute().getDomainAttribute().equals(attribute)) {
							final var msg = msgPrefix + "by a mapping attribute of method '" + m.getName() + "()' in class '" + b.getName()
									+ "'!";

							throw new IllegalStateException(msg);
						}
				}

				for (final MethodParameter p : m.getMethodParameters()) {
					if (p instanceof final FilterMethodParameter filterParam && filterParam.getDomainAttribute() != null
							&& filterParam.getDomainAttribute().equals(attribute))
						throw new IllegalStateException(
								msgPrefix + "by a filter parameter of method '" + m.getName() + "()' in class '" + b.getName() + "'!");
				}
			}
		}
	}

	/**
	 * Remove indexes and foreign keys of a given column
	 * @param col
	 */
	private void removeIndexesAndForeignKeysOfColumn(DBColumn col) {
		final var indexesToBeRemoved = new BasicEList<DBIndex>();
		final var keysToBeRemoved = new BasicEList<ForeignKey>();

		// Remove the foreign keys
		for (final ForeignKey fk : col.getDatabaseTable().getForeignKeys())
			if (fk.getColumn().equals(col))
				keysToBeRemoved.add(fk);

		keysToBeRemoved.forEach(fk -> col.getDatabaseTable().getForeignKeys().remove(fk));

		// Remove the indexes
		for (final DBIndex i : col.getDatabaseTable().getIndexes())
			for (final DBColumn c : i.getColumns())
				if (c.equals(col))
					indexesToBeRemoved.add(i);

		indexesToBeRemoved.forEach(index -> col.getDatabaseTable().getIndexes().remove(index));
	}

	/**
	 * Remove the association table of the given one-to-many association
	 * @param otm the association from which the table should be removed
	 */
	private void removeOneToManyAssociationTable(OneToManyAssociation otm) {
		if (otm.getTable() != null) {
			// Remove the join table
			final Database db = otm.getTable().getDatabase();
			db.getDatabaseTables().remove(otm.getTable());
		}
	}

	/**
	 * Remove the association column of the given one-to-one association
	 * @param oto the association from which the column should be removed
	 */
	private void removeOneToOneAssociationColumn(OneToOneAssociation oto) {
		removeIndexesAndForeignKeysOfColumn(oto.getColumn());
		oto.getColumn().getDatabaseTable().getColumns().remove(oto.getColumn());
	}

	/**
	 * Remove the association column of the given many-to-one association
	 * @param mto the association from which the column should be removed
	 */
	private void removeManyToOneAssociationColumn(ManyToOneAssociation mto) {
		removeIndexesAndForeignKeysOfColumn(mto.getColumn());

		mto.getColumn().getDatabaseTable().getColumns().remove(mto.getColumn());
	}

	/**
	 * Remove the association table of the given many-to-many association
	 * @param mtm the association from which the table should be removed
	 */
	private void removeManyToManyAssociationTable(ManyToManyAssociation mtm) {
		mtm.getTable().getDatabase().getDatabaseTables().remove(mtm.getTable());
	}

}
