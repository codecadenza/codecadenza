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
package net.codecadenza.eclipse.model.boundary;

import net.codecadenza.eclipse.model.java.JavaFile;
import net.codecadenza.eclipse.model.java.JavaType;
import net.codecadenza.eclipse.model.project.WorkspaceFile;
import net.codecadenza.eclipse.model.repository.Repository;
import net.codecadenza.eclipse.model.service.ServiceBean;
import org.eclipse.emf.common.util.EList;

/**
 * A representation of the model object '<em><b>Boundary Bean</b></em>'.
 * <p>
 * The following features are supported:
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.boundary.BoundaryBean#getBoundaryMethods <em>Boundary Methods</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.boundary.BoundaryBean#getRepository <em>Repository</em>}</li>
 * </ul>
 * </p>
 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryBean()
 * @model
 * @generated
 */
public interface BoundaryBean extends ServiceBean {
	/**
	 * Return the value of the '<em><b>Boundary Methods</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.boundary.BoundaryMethod}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.boundary.BoundaryMethod#getBoundaryBean <em>Boundary Bean</em>}'.
	 * @return the value of the '<em>Boundary Methods</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryBean_BoundaryMethods()
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryMethod#getBoundaryBean
	 * @model opposite="boundaryBean" containment="true"
	 * @generated
	 */
	EList<BoundaryMethod> getBoundaryMethods();

	/**
	 * Return the value of the '<em><b>Repository</b></em>' reference
	 * @return the value of the '<em>Repository</em>' reference
	 * @see #setRepository(Repository)
	 * @see net.codecadenza.eclipse.model.boundary.BoundaryPackage#getBoundaryBean_Repository()
	 * @model
	 * @generated
	 */
	Repository getRepository();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.boundary.BoundaryBean#getRepository <em>Repository</em>}'
	 * reference
	 * @param value the new value of the '<em>Repository</em>' reference
	 * @see #getRepository()
	 * @generated
	 */
	void setRepository(Repository value);

	/**
	 * @param returnType
	 * @param methodType
	 * @return the respective boundary method
	 * @throws IllegalStateException if a boundary method could not be found
	 * @generated not
	 */
	BoundaryMethod getBoundaryMethodByReturnType(JavaType returnType, BoundaryMethodTypeEnumeration methodType);

	/**
	 * @return the internal representation of the boundary bean source file
	 * @generated not
	 */
	JavaFile getBeanSourceFile();

	/**
	 * @return the internal representation of the boundary interface source file
	 * @generated not
	 */
	JavaFile getInterfaceSourceFile();

	/**
	 * @return the internal representation of the source file for an Angular application
	 * @generated not
	 */
	WorkspaceFile getTypeScriptSourceFile();
}
