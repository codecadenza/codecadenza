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
package net.codecadenza.eclipse.diagram.domain.part;

import net.codecadenza.eclipse.model.boundary.BoundaryPackage;
import net.codecadenza.eclipse.model.client.ClientPackage;
import net.codecadenza.eclipse.model.db.DbPackage;
import net.codecadenza.eclipse.model.domain.DomainPackage;
import net.codecadenza.eclipse.model.dto.DtoPackage;
import net.codecadenza.eclipse.model.java.JavaPackage;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.repository.RepositoryPackage;
import org.eclipse.core.expressions.PropertyTester;
import org.eclipse.emf.ecore.EClass;
import org.eclipse.emf.ecore.EObject;

/**
 * <p>
 * Domain model element tester
 * </p>
 * <p>
 * Copyright 2025 (C) by Martin Ganserer
 * </p>
 * @author Martin Ganserer
 * @version 1.0.0
 */
public class CodeCadenzaDomainModelElementTester extends PropertyTester {
	/*
	 * (non-Javadoc)
	 * @see org.eclipse.core.expressions.IPropertyTester#test(java.lang.Object, java.lang.String, java.lang.Object[],
	 * java.lang.Object)
	 */
	@Override
	public boolean test(Object receiver, String method, Object[] args, Object expectedValue) {
		if (!(receiver instanceof final EObject eObject))
			return false;

		final EClass eClass = eObject.eClass();

		if (eClass == BoundaryPackage.eINSTANCE.getBoundaryBean())
			return true;

		if (eClass == BoundaryPackage.eINSTANCE.getBoundaryMethod())
			return true;

		if (eClass == ClientPackage.eINSTANCE.getForm())
			return true;

		if (eClass == ClientPackage.eINSTANCE.getFormAction())
			return true;

		if (eClass == ClientPackage.eINSTANCE.getFormField())
			return true;

		if (eClass == ClientPackage.eINSTANCE.getFormGroup())
			return true;

		if (eClass == ClientPackage.eINSTANCE.getFormPanel())
			return true;

		if (eClass == ClientPackage.eINSTANCE.getFormTable())
			return true;

		if (eClass == ClientPackage.eINSTANCE.getTableColumnField())
			return true;

		if (eClass == RepositoryPackage.eINSTANCE.getRepository())
			return true;

		if (eClass == RepositoryPackage.eINSTANCE.getRepositoryMethod())
			return true;

		if (eClass == DbPackage.eINSTANCE.getDBColumn())
			return true;

		if (eClass == DbPackage.eINSTANCE.getDBColumnType())
			return true;

		if (eClass == DbPackage.eINSTANCE.getDBIndex())
			return true;

		if (eClass == DbPackage.eINSTANCE.getDBTable())
			return true;

		if (eClass == DbPackage.eINSTANCE.getDatabase())
			return true;

		if (eClass == DbPackage.eINSTANCE.getForeignKey())
			return true;

		if (eClass == DbPackage.eINSTANCE.getPrimaryKey())
			return true;

		if (eClass == DtoPackage.eINSTANCE.getDTOBean())
			return true;

		if (eClass == DtoPackage.eINSTANCE.getDTOBeanAttribute())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getAbstractDomainAssociation())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getDomainObject())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getDomainAttribute())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getDomainAttributeValidator())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getDomainInheritance())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getDomainNamespace())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getEnumAssociation())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getIDGenerator())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getManyToManyAssociation())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getManyToOneAssociation())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getOneToManyAssociation())
			return true;

		if (eClass == DomainPackage.eINSTANCE.getOneToOneAssociation())
			return true;

		if (eClass == JavaPackage.eINSTANCE.getEnumLiteral())
			return true;

		if (eClass == JavaPackage.eINSTANCE.getJavaEnum())
			return true;

		if (eClass == JavaPackage.eINSTANCE.getJavaMethod())
			return true;

		if (eClass == JavaPackage.eINSTANCE.getJavaType())
			return true;

		if (eClass == JavaPackage.eINSTANCE.getMethodParameter())
			return true;

		if (eClass == JavaPackage.eINSTANCE.getNamespace())
			return true;

		if (eClass == ProjectPackage.eINSTANCE.getDatasource())
			return true;

		if (eClass == ProjectPackage.eINSTANCE.getPersistenceUnitProperty())
			return true;

		if (eClass == ProjectPackage.eINSTANCE.getProject())
			return true;

		return eClass == ProjectPackage.eINSTANCE.getRole();
	}

}
