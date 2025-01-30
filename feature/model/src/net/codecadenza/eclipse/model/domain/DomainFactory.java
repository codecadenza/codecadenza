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
package net.codecadenza.eclipse.model.domain;

import org.eclipse.emf.ecore.EFactory;

/**
 * The factory for meta-model objects of package <b>Domain</b>. It provides a create method for each non-abstract class of the
 * model.
 * @see net.codecadenza.eclipse.model.domain.DomainPackage
 * @generated
 */
public interface DomainFactory extends EFactory {
	/**
	 * The singleton instance of the factory
	 * @generated
	 */
	DomainFactory eINSTANCE = net.codecadenza.eclipse.model.domain.impl.DomainFactoryImpl.init();

	/**
	 * @return a new {@link DomainObject} object
	 * @generated
	 */
	DomainObject createDomainObject();

	/**
	 * @return a new {@link DomainAttribute} object
	 * @generated
	 */
	DomainAttribute createDomainAttribute();

	/**
	 * @return a new {@link DomainAttributeValidator} object
	 * @generated
	 */
	DomainAttributeValidator createDomainAttributeValidator();

	/**
	 * @return a new {@link DomainInheritance} object
	 * @generated
	 */
	DomainInheritance createDomainInheritance();

	/**
	 * @return a new {@link DomainNamespace} object
	 * @generated
	 */
	DomainNamespace createDomainNamespace();

	/**
	 * @return a new {@link EnumAssociation} object
	 * @generated
	 */
	EnumAssociation createEnumAssociation();

	/**
	 * @return a new {@link IDGenerator} object
	 * @generated
	 */
	IDGenerator createIDGenerator();

	/**
	 * @return a new {@link ManyToManyAssociation} object
	 * @generated
	 */
	ManyToManyAssociation createManyToManyAssociation();

	/**
	 * @return a new {@link ManyToOneAssociation} object
	 * @generated
	 */
	ManyToOneAssociation createManyToOneAssociation();

	/**
	 * @return a new {@link OneToManyAssociation} object
	 * @generated
	 */
	OneToManyAssociation createOneToManyAssociation();

	/**
	 * @return a new {@link OneToOneAssociation} object
	 * @generated
	 */
	OneToOneAssociation createOneToOneAssociation();

	/**
	 * @return the package supported by this factory
	 * @generated
	 */
	DomainPackage getDomainPackage();

}
