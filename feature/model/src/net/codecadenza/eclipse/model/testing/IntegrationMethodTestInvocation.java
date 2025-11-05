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
package net.codecadenza.eclipse.model.testing;

import java.util.List;
import java.util.regex.Pattern;
import net.codecadenza.eclipse.model.integration.AbstractIntegrationMethod;
import net.codecadenza.eclipse.model.mapping.MappingAttribute;
import org.eclipse.emf.common.util.EList;
import org.eclipse.emf.ecore.EObject;

/**
 * A representation of the model object '<em><b>Integration Method Test Invocation</b></em>'.
 * <p>
 * The following features are supported:
 * </p>
 * <ul>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectToFail <em>Expect To Fail</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingStatement <em>Post Processing
 * Statement</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTimeout <em>Timeout</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getIntegrationMethod <em>Integration
 * Method</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParameters <em>Parameters</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getReturnValues <em>Return Values</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingAttributes <em>Post
 * Processing Attributes</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getNestedInvocations <em>Nested
 * Invocations</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParentInvocation <em>Parent
 * Invocation</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTestMethodName <em>Test Method
 * Name</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSize <em>Expected Size</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSizeOperator <em>Expected Size
 * Operator</em>}</li>
 * <li>{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectedReturnNull <em>Expected Return
 * Null</em>}</li>
 * </ul>
 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation()
 * @model
 * @generated
 */
public interface IntegrationMethodTestInvocation extends EObject {
	Pattern PLACEHOLDER_PATTERN = Pattern.compile(":([a-zA-Z_$][a-zA-Z0-9_$]*)");

	/**
	 * Return the value of the '<em><b>Expect To Fail</b></em>' attribute
	 * @return the value of the '<em>Expect To Fail</em>' attribute
	 * @see #setExpectToFail(boolean)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_ExpectToFail()
	 * @model
	 * @generated
	 */
	boolean isExpectToFail();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectToFail <em>Expect
	 * To Fail</em>}' attribute
	 * @param value the new value of the '<em>Expect To Fail</em>' attribute
	 * @see #isExpectToFail()
	 * @generated
	 */
	void setExpectToFail(boolean value);

	/**
	 * Return the value of the '<em><b>Post Processing Statement</b></em>' attribute
	 * @return the value of the '<em>Post Processing Statement</em>' attribute
	 * @see #setPostProcessingStatement(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_PostProcessingStatement()
	 * @model
	 * @generated
	 */
	String getPostProcessingStatement();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getPostProcessingStatement
	 * <em>Post Processing Statement</em>}' attribute
	 * @param value the new value of the '<em>Post Processing Statement</em>' attribute
	 * @see #getPostProcessingStatement()
	 * @generated
	 */
	void setPostProcessingStatement(String value);

	/**
	 * Return the value of the '<em><b>Timeout</b></em>' attribute
	 * @return the value of the '<em>Timeout</em>' attribute
	 * @see #setTimeout(Integer)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_Timeout()
	 * @model
	 * @generated
	 */
	Integer getTimeout();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTimeout
	 * <em>Timeout</em>}' attribute
	 * @param value the new value of the '<em>Timeout</em>' attribute
	 * @see #getTimeout()
	 * @generated
	 */
	void setTimeout(Integer value);

	/**
	 * Return the value of the '<em><b>Integration Method</b></em>' reference
	 * @return the value of the '<em>Integration Method</em>' reference
	 * @see #setIntegrationMethod(AbstractIntegrationMethod)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_IntegrationMethod()
	 * @model
	 * @generated
	 */
	AbstractIntegrationMethod getIntegrationMethod();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getIntegrationMethod
	 * <em>Integration Method</em>}' reference
	 * @param value the new value of the '<em>Integration Method</em>' reference
	 * @see #getIntegrationMethod()
	 * @generated
	 */
	void setIntegrationMethod(AbstractIntegrationMethod value);

	/**
	 * Return the value of the '<em><b>Parameters</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.MethodInvocationParameter}
	 * @return the value of the '<em>Parameters</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_Parameters()
	 * @model containment="true"
	 * @generated
	 */
	EList<MethodInvocationParameter> getParameters();

	/**
	 * Return the value of the '<em><b>Return Values</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.TestDataObject}.
	 * @return the value of the '<em>Return Values</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_ReturnValues()
	 * @model containment="true"
	 * @generated
	 */
	EList<TestDataObject> getReturnValues();

	/**
	 * Return the value of the '<em><b>Post Processing Attributes</b></em>' reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.TestDataAttribute}.
	 * @return the value of the '<em>Post Processing Attributes</em>' reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_PostProcessingAttributes()
	 * @model
	 * @generated
	 */
	EList<TestDataAttribute> getPostProcessingAttributes();

	/**
	 * Return the value of the '<em><b>Nested Invocations</b></em>' containment reference list. The list contents are of type
	 * {@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation}. It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParentInvocation <em>Parent
	 * Invocation</em>}'.
	 * @return the value of the '<em>Nested Invocations</em>' containment reference list
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_NestedInvocations()
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParentInvocation
	 * @model opposite="parentInvocation" containment="true"
	 * @generated
	 */
	EList<IntegrationMethodTestInvocation> getNestedInvocations();

	/**
	 * Return the value of the '<em><b>Parent Invocation</b></em>' container reference It is bidirectional and its opposite is
	 * '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getNestedInvocations <em>Nested
	 * Invocations</em>}'
	 * @return the value of the '<em>Parent Invocation</em>' container reference
	 * @see #setParentInvocation(IntegrationMethodTestInvocation)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_ParentInvocation()
	 * @see net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getNestedInvocations
	 * @model opposite="nestedInvocations" transient="false"
	 * @generated
	 */
	IntegrationMethodTestInvocation getParentInvocation();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getParentInvocation
	 * <em>Parent Invocation</em>}' container reference
	 * @param value the new value of the '<em>Parent Invocation</em>' container reference
	 * @see #getParentInvocation()
	 * @generated
	 */
	void setParentInvocation(IntegrationMethodTestInvocation value);

	/**
	 * Return the value of the '<em><b>Test Method Name</b></em>' attribute
	 * @return the value of the '<em>Test Method Name</em>' attribute
	 * @see #setTestMethodName(String)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_TestMethodName()
	 * @model
	 * @generated
	 */
	String getTestMethodName();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getTestMethodName <em>Test
	 * Method Name</em>}' attribute
	 * @param value the new value of the '<em>Test Method Name</em>' attribute
	 * @see #getTestMethodName()
	 * @generated
	 */
	void setTestMethodName(String value);

	/**
	 * Return the value of the '<em><b>Expected Size</b></em>' attribute
	 * @return the value of the '<em>Expected Size</em>' attribute
	 * @see #setExpectedSize(Integer)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_ExpectedSize()
	 * @model
	 * @generated
	 */
	Integer getExpectedSize();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.TestDataAttribute#getExpectedSize <em>Expected Size</em>}'
	 * attribute
	 * @param value the new value of the '<em>Expected Size</em>' attribute
	 * @see #getExpectedSize()
	 * @generated
	 */
	void setExpectedSize(Integer value);

	/**
	 * Return the value of the '<em><b>Expected Size Operator</b></em>' attribute. The literals are from the enumeration
	 * {@link net.codecadenza.eclipse.model.testing.AssertionOperator}.
	 * @return the value of the '<em>Expected Size Operator</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
	 * @see #setExpectedSizeOperator(AssertionOperator)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_ExpectedSizeOperator()
	 * @model
	 * @generated
	 */
	AssertionOperator getExpectedSizeOperator();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#getExpectedSizeOperator
	 * <em>Expected Size Operator</em>}' attribute
	 * @param value the new value of the '<em>Expected Size Operator</em>' attribute
	 * @see net.codecadenza.eclipse.model.testing.AssertionOperator
	 * @see #getExpectedSizeOperator()
	 * @generated
	 */
	void setExpectedSizeOperator(AssertionOperator value);

	/**
	 * Return the value of the '<em><b>Expected Return Null</b></em>' attribute
	 * @return the value of the '<em>Expected Return Null</em>' attribute
	 * @see #setExpectedReturnNull(boolean)
	 * @see net.codecadenza.eclipse.model.testing.TestingPackage#getIntegrationMethodTestInvocation_ExpectedReturnNull()
	 * @model
	 * @generated
	 */
	boolean isExpectedReturnNull();

	/**
	 * Set the value of the '{@link net.codecadenza.eclipse.model.testing.IntegrationMethodTestInvocation#isExpectedReturnNull
	 * <em>Expected Return Null</em>}' attribute
	 * @param value the new value of the '<em>Expected Return Null</em>' attribute
	 * @see #isExpectedReturnNull()
	 * @generated
	 */
	void setExpectedReturnNull(boolean value);

	/**
	 * @return true if the respective method returns a list
	 * @generated not
	 */
	boolean isReturnList();

	/**
	 * @return true if the respective method returns void
	 * @generated not
	 */
	boolean isReturnVoid();

	/**
	 * @return the tracked attribute or null if no tracking attribute exists
	 * @generated not
	 */
	TestDataAttribute getTrackedAttribute();

	/**
	 * @return the list with all attributes of this method invocation
	 * @generated not
	 */
	List<TestDataAttribute> getAllAttributes();

	/**
	 * @param mappingAttribute
	 * @return a set of IDs for all {@link TestDataAttribute}s that are mapped to the given mapping attribute
	 * @generated not
	 */
	List<String> getIdsOfTrackedAttributes(MappingAttribute mappingAttribute);

	/**
	 * @return a list of IDs for all {@link TestDataAttribute}s that provide an expected size
	 * @generated not
	 */
	List<String> getIdsOfExpectedSizeFields();

	/**
	 * @return a list with all attribute names from the given post-processing statement
	 * @generated not
	 */
	List<String> extractAttributeNamesFromStatement();

	/**
	 * @return if the invocation needs to either upload or download a file
	 * @generated not
	 */
	boolean isFileHandlingRequired();

	/**
	 * @return true if a file should be uploaded
	 * @generated not
	 */
	boolean isUploadFile();

	/**
	 * @return true if a file should be downloaded
	 * @generated not
	 */
	boolean isDownloadFile();

	/**
	 * @return the {@link TestDataAttribute} that contains the path of a file
	 * @generated not
	 */
	TestDataAttribute getFilePathAttribute();

	/**
	 * @return the {@link TestDataAttribute} that contains the name of a file
	 * @generated not
	 */
	TestDataAttribute getFileNameAttribute();

	/**
	 * @return true if a parameter that controls the time to wait for a response of an asynchronous invocation is necessary
	 * @generated not
	 */
	boolean isWaitForResponseParameterRequired();

	/**
	 * @return true if the corresponding integration method can return null
	 * @generated not
	 */
	boolean canReturnNull();

}
