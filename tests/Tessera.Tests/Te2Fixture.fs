/// A database.json written by Tabular Editor 2 with LocalTranslations, LocalPerspectives and
/// LocalRelationships set: DatabaseJsonCompare from TE2's
/// TOMWrapperTest/RegressionTests/v2_8/SerializationAnnotationsTests.cs, with LF line endings.
/// Copyright (c) 2017 Daniel Otykier. MIT License: https://github.com/TabularEditor/TabularEditor
module Tessera.Tests.Te2Fixture

let databaseJson =
    """{
  "name": "New Tabular Database",
  "compatibilityLevel": 1400,
  "model": {
    "dataSources": [
      {
        "name": "New Provider Data Source",
        "impersonationMode": "impersonateServiceAccount"
      }
    ],
    "tables": [
      {
        "name": "Table 1",
        "columns": [
          {
            "name": "Column 1",
            "dataType": "string",
            "annotations": [
              {
                "name": "TabularEditor_TranslatedNames",
                "value": "{\"da-DK\":\"Test 1\"}"
              },
              {
                "name": "TabularEditor_InPerspective",
                "value": "[\"Perspective 1\"]"
              }
            ]
          },
          {
            "name": "Column 2",
            "dataType": "string"
          },
          {
            "name": "Column 3",
            "dataType": "string"
          }
        ],
        "partitions": [
          {
            "name": "Table 1",
            "source": {
              "type": "query",
              "dataSource": "New Provider Data Source"
            }
          }
        ],
        "annotations": [
          {
            "name": "TabularEditor_InPerspective",
            "value": "[\"Perspective 1\"]"
          },
          {
            "name": "TabularEditor_Relationships",
            "value": [
              "[",
              "  {",
              "    \"fromTable\": \"Table 1\",",
              "    \"fromColumn\": \"Column 1\",",
              "    \"toTable\": \"Table 2\",",
              "    \"toColumn\": \"Column 1\"",
              "  }",
              "]"
            ]
          }
        ]
      },
      {
        "name": "Table 2",
        "columns": [
          {
            "name": "Column 1",
            "dataType": "string"
          },
          {
            "name": "Column 2",
            "dataType": "string",
            "annotations": [
              {
                "name": "TabularEditor_InPerspective",
                "value": "[\"Perspective 2\"]"
              }
            ]
          },
          {
            "name": "Column 3",
            "dataType": "string",
            "annotations": [
              {
                "name": "TabularEditor_InPerspective",
                "value": "[\"Perspective 2\"]"
              }
            ]
          }
        ],
        "partitions": [
          {
            "name": "Table 2",
            "source": {
              "type": "query",
              "dataSource": "New Provider Data Source"
            }
          }
        ],
        "annotations": [
          {
            "name": "TabularEditor_InPerspective",
            "value": "[\"Perspective 2\"]"
          },
          {
            "name": "TabularEditor_Relationships",
            "value": "[]"
          }
        ]
      }
    ],
    "annotations": [
      {
        "name": "TabularEditor_SerializeOptions",
        "value": "{\"IgnoreInferredObjects\":true,\"IgnoreInferredProperties\":true,\"IgnoreTimestamps\":true,\"SplitMultilineStrings\":true,\"PrefixFilenames\":false,\"LocalTranslations\":true,\"LocalPerspectives\":true,\"LocalRelationships\":true,\"Levels\":[]}"
      },
      {
        "name": "TabularEditor_Cultures",
        "value": "[\"da-DK\",\"en-US\"]"
      },
      {
        "name": "TabularEditor_Perspectives",
        "value": "[{\"Name\":\"Perspective 1\",\"Description\":\"\",\"Annotations\":{}},{\"Name\":\"Perspective 2\",\"Description\":\"\",\"Annotations\":{}}]"
      }
    ]
  }
}"""
