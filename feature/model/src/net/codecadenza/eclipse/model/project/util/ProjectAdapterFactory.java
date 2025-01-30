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
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.common.notify.Notifier;
import org.eclipse.emf.common.notify.impl.AdapterFactoryImpl;
import org.eclipse.emf.ecore.EObject;

/**
 * The <b>Adapter Factory</b> for the model. It provides an adapter <code>createXXX</code> method for each class of the model.
 * @see net.codecadenza.eclipse.model.project.ProjectPackage
 * @generated
 */
public class ProjectAdapterFactory extends AdapterFactoryImpl {
	/**
	 * The cached model package
	 * @generated
	 */
	protected static ProjectPackage modelPackage;

	/**
	 * Create an instance of the adapter factory
	 * @generated
	 */
	public ProjectAdapterFactory() {
		if (modelPackage == null)
			modelPackage = ProjectPackage.eINSTANCE;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#isFactoryForType(java.lang.Object)
	 * @generated
	 */
	@Override
	public boolean isFactoryForType(Object object) {
		if (object == modelPackage)
			return true;

		if (object instanceof final EObject eObject)
			return eObject.eClass().getEPackage() == modelPackage;

		return false;
	}

	/**
	 * The switch that delegates to the <code>createXXX</code> methods.
	 * @generated
	 */
	protected ProjectSwitch<Adapter> modelSwitch = new ProjectSwitch<>() {
		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.project.util.ProjectSwitch#caseDatasource(net.codecadenza.eclipse.model.project.
		 * Datasource)
		 */
		@Override
		public Adapter caseDatasource(Datasource object) {
			return createDatasourceAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.project.util.ProjectSwitch#casePersistenceUnitProperty(net.codecadenza.eclipse.model.
		 * project.PersistenceUnitProperty)
		 */
		@Override
		public Adapter casePersistenceUnitProperty(PersistenceUnitProperty object) {
			return createPersistenceUnitPropertyAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.project.util.ProjectSwitch#caseProject(net.codecadenza.eclipse.model.project.Project)
		 */
		@Override
		public Adapter caseProject(Project object) {
			return createProjectAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.project.util.ProjectSwitch#caseRole(net.codecadenza.eclipse.model.project.Role)
		 */
		@Override
		public Adapter caseRole(Role object) {
			return createRoleAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.project.util.ProjectSwitch#caseBuildArtifact(net.codecadenza.eclipse.model.project.
		 * BuildArtifact)
		 */
		@Override
		public Adapter caseBuildArtifact(BuildArtifact object) {
			return createBuildArtifactAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.project.util.ProjectSwitch#caseIntegrationModule(net.codecadenza.eclipse.model.project.
		 * IntegrationModule)
		 */
		@Override
		public Adapter caseIntegrationModule(IntegrationModule object) {
			return createIntegrationModuleAdapter();
		}

		/*
		 * (non-Javadoc)
		 * @see net.codecadenza.eclipse.model.project.util.ProjectSwitch#defaultCase(org.eclipse.emf.ecore.EObject)
		 */
		@Override
		public Adapter defaultCase(EObject object) {
			return createEObjectAdapter();
		}
	};

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.emf.common.notify.impl.AdapterFactoryImpl#createAdapter(org.eclipse.emf.common.notify.Notifier)
	 * @generated
	 */
	@Override
	public Adapter createAdapter(Notifier target) {
		return modelSwitch.doSwitch((EObject) target);
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.project.Datasource <em>Datasource</em>}'.
	 * This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance
	 * will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.project.Datasource
	 * @generated
	 */
	public Adapter createDatasourceAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.project.PersistenceUnitProperty
	 * <em>Persistence Unit Property</em>}'. This default implementation returns null so that we can easily ignore cases; it's
	 * useful to ignore a case when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.project.PersistenceUnitProperty
	 * @generated
	 */
	public Adapter createPersistenceUnitPropertyAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.project.Project <em>Project</em>}'. This
	 * default implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will
	 * catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.project.Project
	 * @generated
	 */
	public Adapter createProjectAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.project.Role <em>Role</em>}'. This default
	 * implementation returns null so that we can easily ignore cases; it's useful to ignore a case when inheritance will catch all
	 * the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.project.Role
	 * @generated
	 */
	public Adapter createRoleAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.project.BuildArtifact <em>Build
	 * Artifact</em>}'. This default implementation returns null so that we can easily ignore cases; it's useful to ignore a case
	 * when inheritance will catch all the cases anyway.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.project.BuildArtifact
	 * @generated
	 */
	public Adapter createBuildArtifactAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for an object of class '{@link net.codecadenza.eclipse.model.project.IntegrationModule <em>Integration
	 * Module</em>}'.
	 * @return the new adapter
	 * @see net.codecadenza.eclipse.model.project.IntegrationModule
	 * @generated
	 */
	public Adapter createIntegrationModuleAdapter() {
		return null;
	}

	/**
	 * Create a new adapter for the default case. This default implementation returns null.
	 * @return the new adapter
	 * @generated
	 */
	public Adapter createEObjectAdapter() {
		return null;
	}

}
