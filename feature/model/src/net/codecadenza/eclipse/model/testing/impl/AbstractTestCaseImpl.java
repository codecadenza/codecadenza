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
package net.codecadenza.eclipse.model.testing.impl;

import net.codecadenza.eclipse.model.java.impl.JavaTypeImpl;
import net.codecadenza.eclipse.model.testing.AbstractTestCase;
import net.codecadenza.eclipse.model.testing.AbstractTestModule;
import net.codecadenza.eclipse.model.testing.TestingPackage;
import org.eclipse.emf.ecore.EClass;

/**
 * An implementation of the model object '<em><b>Abstract Test Case</b></em>'.
 * @generated
 */
public abstract class AbstractTestCaseImpl extends JavaTypeImpl implements AbstractTestCase {
	/**
	 * @generated
	 */
	protected AbstractTestCaseImpl() {
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.java.impl.JavaTypeImpl#eStaticClass()
	 * @generated
	 */
	@Override
	protected EClass eStaticClass() {
		return TestingPackage.Literals.ABSTRACT_TEST_CASE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.codecadenza.eclipse.model.testing.AbstractTestCase#getTestModule()
	 * @generated not
	 */
	@Override
	public AbstractTestModule getTestModule() {
		return getNamespace().getProject().getTestModules().stream()
				.filter(testModule -> testModule.getNamespace().equals(getNamespace())).findFirst().orElse(null);
	}

}
