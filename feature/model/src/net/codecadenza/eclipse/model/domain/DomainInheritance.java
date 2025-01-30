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

import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Inheritance</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainInheritance#getSource <em>Source</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.domain.DomainInheritance#getTarget <em>Target</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainInheritance()
 * @model
 * @generated
 */
public interface DomainInheritance extends EObject {
	/**
	 * Return the value of the '<em><b>Source</b></em>' container reference. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.domain.DomainObject#getInheritance <em>Inheritance</em>}'.
	 * @return the value of the '<em>Source</em>' container reference
	 * @see #setSource(DomainObject)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainInheritance_Source()
	 * @see net.codecadenza.eclipse.model.domain.DomainObject#getInheritance
	 * @model opposite="inheritance"
	 * @generated
	 */
	DomainObject getSource();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainInheritance#getSource <em>Source</em>}' container
	 * reference
	 * @param value the new value of the '<em>Source</em>' container reference
	 * @see #getSource()
	 * @generated
	 */
	void setSource(DomainObject value);

	/**
	 * Return the value of the '<em><b>Target</b></em>' reference
	 * @return the value of the '<em>Target</em>' reference
	 * @see #setTarget(DomainObject)
	 * @see net.codecadenza.eclipse.model.domain.DomainPackage#getDomainInheritance_Target()
	 * @model
	 * @generated
	 */
	DomainObject getTarget();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.domain.DomainInheritance#getTarget <em>Target</em>}' reference
	 * @param value the new value of the '<em>Target</em>' reference
	 * @see #getTarget()
	 * @generated
	 */
	void setTarget(DomainObject value);

}
