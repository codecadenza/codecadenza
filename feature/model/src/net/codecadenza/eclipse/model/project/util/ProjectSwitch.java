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
package net.codecadenza.eclipse.model.project.util;

import net.codecadenza.eclipse.model.project.BuildArtifact;
import net.codecadenza.eclipse.model.project.Datasource;
import net.codecadenza.eclipse.model.project.IntegrationModule;
import net.codecadenza.eclipse.model.project.PersistenceUnitProperty;
import net.codecadenza.eclipse.model.project.Project;
import net.codecadenza.eclipse.model.project.ProjectPackage;
import net.codecadenza.eclipse.model.project.Role;
import org.eclipse.emf.ecore.EObject;
import org.eclipse.emf.ecore.EPackage;
import org.eclipse.emf.ecore.util.Switch;

/**
 * The <b>Switch</b> for the model's inheritance hierarchy. It supports the call {@link #doSwitch(EObject) doSwitch(object)} to
 * invoke the <code>caseXXX</code> method for each class of the model, starting with the actual class of the object and proceeding
 * up the inheritance hierarchy until a non-null result is returned, which is the result of the switch.
 * @param <T> the type of the <b>Switch</b>
 * @see net.codecadenza.eclipse.model.project.ProjectPackage
 * @generated
 */
public class ProjectSwitch<T> extends Switch<T> {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static ProjectPackage modelPackage;

	/**
	 * Create an instance of the switch
	 * @generated
	 */
	public ProjectSwitch() {
		if (modelPackage == null)
			modelPackage = ProjectPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#isSwitchFor(org.eclipse.emf.ecore.EPackage)
	 * @generated
	 */
	@Override
	protected boolean isSwitchFor(EPackage ePackage) {
		return ePackage == modelPackage;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#doSwitch(int, org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	protected T doSwitch(int classifierID, EObject theEObject) {
		switch (classifierID) {
			case ProjectPackage.DATASOURCE: {
				final var datasource = (Datasource) theEObject;
				T result = caseDatasource(datasource);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ProjectPackage.PERSISTENCE_UNIT_PROPERTY: {
				final var persistenceUnitProperty = (PersistenceUnitProperty) theEObject;
				T result = casePersistenceUnitProperty(persistenceUnitProperty);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ProjectPackage.PROJECT: {
				final var project = (Project) theEObject;
				T result = caseProject(project);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ProjectPackage.ROLE: {
				final var role = (Role) theEObject;
				T result = caseRole(role);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ProjectPackage.BUILD_ARTIFACT: {
				final var buildArtifact = (BuildArtifact) theEObject;
				T result = caseBuildArtifact(buildArtifact);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			case ProjectPackage.INTEGRATION_MODULE: {
				final var integrationModule = (IntegrationModule) theEObject;
				T result = caseIntegrationModule(integrationModule);

				if (result == null)
					result = defaultCase(theEObject);

				return result;
			}
			default:
				return defaultCase(theEObject);
		}
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Datasource</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Datasource</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseDatasource(Datasource object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Persistence Unit Property</em>'. This implementation
	 * returns null; returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Persistence Unit Property</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T casePersistenceUnitProperty(PersistenceUnitProperty object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Project</em>'. This implementation returns null;
	 * returning a non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Project</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseProject(Project object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Role</em>'. This implementation returns null; returning a
	 * non-null result will terminate the switch.
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Role</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseRole(Role object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Build Artifact</em>'
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Build Artifact</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseBuildArtifact(BuildArtifact object) {
		return null;
	}

	/**
	 * Return the result of interpreting the object as an instance of '<em>Integration Module</em>'
	 * @param object the target of the switch
	 * @return the result of interpreting the object as an instance of '<em>Integration Module</em>'
	 * @see #doSwitch(org.eclipse.emf.ecore.EObject) doSwitch(EObject)
	 * @generated
	 */
	@SuppressWarnings("unused")
	public T caseIntegrationModule(IntegrationModule object) {
		return null;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.ecore.util.Switch#defaultCase(org.eclipse.emf.ecore.EObject)
	 * @generated
	 */
	@Override
	public T defaultCase(EObject object) {
		return null;
	}

}
